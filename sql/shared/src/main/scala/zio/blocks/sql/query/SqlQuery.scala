/*
 * Copyright 2024-2026 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package zio.blocks.sql.query

import zio.blocks.sql.{DbValue, Frag, SqlDialect, SqlIdentifier, SqlStatement, Table}

/**
 * Immutable query IR starting from a source table.
 *
 * This is the blessed SELECT builder for new code (see the module decision
 * below): joins are validated through typed [[Rel]]s, rendering is performed by
 * [[QueryRenderer]] through `Frag.++` composition only, every identifier is
 * validated and double-quoted, and values always bind as `?` params. Use
 * [[explain]] for human-readable logging and [[statement]] for structured
 * inspection.
 *
 * Alias allocation is deterministic: t0 = source, t1..tN in join order
 * (self-join safe — same Table joined twice gets distinct aliases). Rendering
 * is performed by [[QueryRenderer]] and produces a [[Frag]] via `Frag.++`
 * composition only, using quoted identifiers and dialect placeholders.
 */
final case class SqlQuery[A] private[query] (
  source: Table[A],
  joins: Vector[JoinNode],
  filters: Vector[Frag],
  groupBy: Vector[String],
  having: Option[Frag],
  orderBy: Vector[OrderBy],
  limit: Option[Int],
  offset: Option[Int]
) {

  def innerJoin[From, To](rel: Rel[From, To]): SqlQuery[A] =
    addJoin(rel, JoinKind.Inner)

  def leftJoin[From, To](rel: Rel[From, To]): SqlQuery[A] =
    addJoin(rel, JoinKind.Left)

  def join[From, To](rel: Rel[From, To], kind: JoinKind): SqlQuery[A] =
    addJoin(rel, kind)

  /** Alias for `innerJoin` — satisfies the `SqlQuery.join(rel)` contract. */
  def join[From, To](rel: Rel[From, To]): SqlQuery[A] =
    addJoin(rel, JoinKind.Inner)

  /** Alias for `leftJoin` — satisfies the `SqlQuery.joinLeft(rel)` contract. */
  def joinLeft[From, To](rel: Rel[From, To]): SqlQuery[A] =
    addJoin(rel, JoinKind.Left)

  private def addJoin[From, To](rel: Rel[From, To], kind: JoinKind): SqlQuery[A] = {
    val nextAlias  = s"t${joins.size + 1}"
    val isSelfJoin = rel.fromTable.name == rel.toTable.name
    @scala.annotation.nowarn
    val pair = if (isSelfJoin) {
      // self-join: t0."fk" = tN."pk"
      val fk = SqlIdentifier.validate("column", rel.fkColumn)
      val pk = SqlIdentifier.validate("column", rel.pkColumn)
      val on = Frag.literal(s"""t0."$fk" = $nextAlias."$pk"""")
      (rel.toTable.asInstanceOf[Table[_]], on)
    } else {
      val fromOpt          = aliasOf(rel.fromTable)
      val toOpt            = aliasOf(rel.toTable)
      var target: Table[_] = null.asInstanceOf[Table[_]]
      var fkAlias: String  = ""
      var pkAlias: String  = ""
      (fromOpt, toOpt) match {
        case (Some(fa), None) =>
          target = rel.toTable.asInstanceOf[Table[_]]
          fkAlias = fa
          pkAlias = nextAlias
        case (None, Some(ta)) =>
          target = rel.fromTable.asInstanceOf[Table[_]]
          fkAlias = nextAlias
          pkAlias = ta
        case (None, None) =>
          // Neither side is yet in query — attach fk side to last alias (or t0) and introduce to side as new.
          // Prefer treating fromTable as existing (t0/last) and toTable as new when source matches fromTable by name heuristic.
          // Fallback: attach from side as existing t0, target is toTable.
          // Determine by checking if source name equals fromTable name -> then from is existing.
          // Otherwise treat to as existing if source matches toTable.
          if (source.name == rel.fromTable.name) {
            target = rel.toTable.asInstanceOf[Table[_]]
            fkAlias = "t0"
            pkAlias = nextAlias
          } else if (source.name == rel.toTable.name) {
            target = rel.fromTable.asInstanceOf[Table[_]]
            fkAlias = nextAlias
            pkAlias = "t0"
          } else if (joins.nonEmpty && joins.last.table.name == rel.fromTable.name) {
            target = rel.toTable.asInstanceOf[Table[_]]
            fkAlias = joins.last.alias
            pkAlias = nextAlias
          } else if (joins.nonEmpty && joins.last.table.name == rel.toTable.name) {
            target = rel.fromTable.asInstanceOf[Table[_]]
            fkAlias = nextAlias
            pkAlias = joins.last.alias
          } else {
            // default: from is existing (t0), to is new
            target = rel.toTable.asInstanceOf[Table[_]]
            fkAlias = "t0"
            pkAlias = nextAlias
          }
        case (Some(_), Some(_)) =>
          // Both sides already present — treat as joining duplicate alias for target? Use from as existing and create new alias for target duplicate.
          target = rel.toTable.asInstanceOf[Table[_]]
          fkAlias = fromOpt.get
          pkAlias = nextAlias
      }
      val fk = SqlIdentifier.validate("column", rel.fkColumn)
      val pk = SqlIdentifier.validate("column", rel.pkColumn)
      val on = Frag.literal(s"""$fkAlias."$fk" = $pkAlias."$pk"""")
      (target, on)
    }
    val (targetTable, onFrag) = pair

    val node = JoinNode(targetTable, nextAlias, kind, onFrag)
    copy(joins = joins :+ node)
  }

  def filter(frag: Frag): SqlQuery[A] =
    copy(filters = filters :+ frag)

  def where(frag: Frag): SqlQuery[A] = filter(frag)

  def groupBy(cols: String*): SqlQuery[A] = {
    cols.foreach(c => SqlIdentifier.validate("column", c))
    copy(groupBy = cols.toVector)
  }

  def having(frag: Frag): SqlQuery[A] =
    copy(having = Some(frag))

  def orderBy(col: String, dir: SortOrder = SortOrder.Asc): SqlQuery[A] = {
    SqlIdentifier.validate("column", col)
    copy(orderBy = orderBy :+ OrderBy(col, dir))
  }

  def orderByMany(cols: OrderBy*): SqlQuery[A] =
    copy(orderBy = orderBy ++ cols.toVector)

  def limit(n: Int): SqlQuery[A] = {
    require(n >= 0, "limit must be >= 0")
    copy(limit = Some(n))
  }

  def offset(n: Int): SqlQuery[A] = {
    require(n >= 0, "offset must be >= 0")
    copy(offset = Some(n))
  }

  def toFrag(dialect: SqlDialect): Frag = QueryRenderer.render(this, dialect)

  def sql(dialect: SqlDialect): String = toFrag(dialect).sql(dialect)

  /**
   * Structured, inspectable representation of this query for a specific
   * dialect. Returns a [[SqlStatement]] mirroring the joins, filters, grouping
   * and pagination decomposed into typed fields.
   */
  def statement(dialect: SqlDialect): SqlStatement = {
    val frag = toFrag(dialect)
    val src  = SqlStatement.Source(source.name, "t0")

    val stJoins = joins.map { j =>
      val onStr             = j.on.sql(dialect)
      val Pattern           = """(t\d+)\."(\w+)" = (t\d+)\."(\w+)""".r
      val (onLeft, onRight) = onStr match {
        case Pattern(la, lc, ra, rc) =>
          (SqlStatement.ColumnRef(la, lc), SqlStatement.ColumnRef(ra, rc))
        case _ =>
          (SqlStatement.ColumnRef("t0", "?"), SqlStatement.ColumnRef(j.alias, "?"))
      }
      val kind = j.kind match {
        case JoinKind.Inner => SqlStatement.JoinKind.Inner
        case JoinKind.Left  => SqlStatement.JoinKind.Left
      }
      SqlStatement.Join(kind, j.table.name, j.alias, onLeft, onRight)
    }

    def resolveColRef(col: String): SqlStatement.ColumnRef =
      if (source.columns.contains(col)) SqlStatement.ColumnRef("t0", col)
      else
        joins.find(_.table.columns.contains(col)) match {
          case Some(j) => SqlStatement.ColumnRef(j.alias, col)
          case None    => SqlStatement.ColumnRef("t0", col)
        }

    val stFilters = frag.params.zipWithIndex.map { case (param, idx) =>
      val parts     = frag.parts
      val sqlBefore = if (idx < parts.length) parts(idx) else ""
      val lastDot   = sqlBefore.lastIndexOf('.')
      val lastSpace = sqlBefore.lastIndexOf(' ')
      val alias     =
        if (lastDot > 0 && lastSpace < lastDot) sqlBefore.substring(lastSpace + 1, lastDot).trim
        else "t0"
      val column =
        if (lastDot > 0 && lastSpace < lastDot) sqlBefore.substring(lastDot + 1).trim.replaceAll("[^a-zA-Z0-9_]", "")
        else "id"
      val sqlAfter = if (idx + 1 < parts.length) parts(idx + 1).trim else ""
      val op       = sqlAfter.split("\\s+").headOption.filter(_.nonEmpty).getOrElse("=")
      SqlStatement.Filter(SqlStatement.ColumnRef(alias, column), op, param)
    }.toVector

    val stGroupBy =
      if (groupBy.isEmpty) None
      else Some(SqlStatement.GroupBy(groupBy.map(resolveColRef).toVector))

    val stOrderBy = orderBy.map { o =>
      SqlStatement.OrderBy(
        resolveColRef(o.column),
        o.direction match {
          case SortOrder.Asc  => SqlStatement.OrderDirection.Asc
          case SortOrder.Desc => SqlStatement.OrderDirection.Desc
        }
      )
    }.toVector

    SqlStatement(
      source = src,
      joins = stJoins,
      filters = stFilters,
      groupBy = stGroupBy,
      orderBy = stOrderBy,
      limit = limit.map(SqlStatement.Limit(_)),
      offset = offset.map(SqlStatement.Offset(_)),
      frag = frag
    )
  }

  /**
   * Renders this query as a human-readable SQL string with `?N`-style numbered
   * placeholders and a `-- params: ...` footer listing each parameter's type.
   * Useful for logging and debugging.
   */
  def explain(dialect: SqlDialect): String = {
    val st   = statement(dialect)
    val frag = st.frag
    val sb   = new StringBuilder
    var idx  = 1
    var i    = 0
    while (i < frag.parts.length) {
      sb.append(frag.parts(i))
      if (i < frag.params.length) {
        sb.append(s"?$idx")
        idx += 1
      }
      i += 1
    }
    val sql = sb.toString()
    if (frag.params.isEmpty) s"$sql\n-- params: (none)"
    else {
      val types = frag.params.zipWithIndex.map { case (v, n) => s"${n + 1}:${typeLabel(v)}" }.mkString(", ")
      s"$sql\n-- params: $types"
    }
  }

  private def typeLabel(v: DbValue): String = v match {
    case DbValue.DbNull             => "Null"
    case _: DbValue.DbInt           => "Int"
    case _: DbValue.DbLong          => "Long"
    case _: DbValue.DbDouble        => "Double"
    case _: DbValue.DbFloat         => "Float"
    case _: DbValue.DbBoolean       => "Boolean"
    case _: DbValue.DbString        => "String"
    case _: DbValue.DbBigDecimal    => "BigDecimal"
    case _: DbValue.DbBytes         => "Bytes"
    case _: DbValue.DbShort         => "Short"
    case _: DbValue.DbByte          => "Byte"
    case _: DbValue.DbChar          => "Char"
    case _: DbValue.DbLocalDate     => "LocalDate"
    case _: DbValue.DbLocalDateTime => "LocalDateTime"
    case _: DbValue.DbLocalTime     => "LocalTime"
    case _: DbValue.DbInstant       => "Instant"
    case _: DbValue.DbDuration      => "Duration"
    case _: DbValue.DbUUID          => "UUID"
    case _: DbValue.DbArray         => "Array"
  }

  private def aliasOf(table: Table[_]): Option[String] =
    if (table.name == source.name) Some("t0")
    else joins.find(_.table.name == table.name).map(_.alias)
}

object SqlQuery {
  def from[A](table: Table[A]): SqlQuery[A] =
    SqlQuery(table, Vector.empty, Vector.empty, Vector.empty, None, Vector.empty, None, None)
}

private[query] final case class JoinNode(
  table: Table[_],
  alias: String,
  kind: JoinKind,
  on: Frag
)

final case class OrderBy(column: String, direction: SortOrder)
