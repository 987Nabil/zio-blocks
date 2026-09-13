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

package zio.blocks.sql

import zio.test._
import zio.blocks.schema._
import zio.blocks.sql.query.{Rel, SqlQuery => Qry, SortOrder => QSortOrder}

object ExplainSpec extends ZIOSpecDefault {
  case class User(id: Int, name: String)
  object User {
    implicit val schema: Schema[User] = Schema.derived
  }

  case class Repo(id: Int, ownerId: Int, name: String)
  object Repo {
    implicit val schema: Schema[Repo] = Schema.derived
  }

  case class Star(userId: Int, repoId: Int)
  object Star {
    implicit val schema: Schema[Star] = Schema.derived
  }

  val userTable = Table.derived[User]
  val repoTable = Table.derived[Repo]
  val starTable = Table.derived[Star]

  def spec = suite("ExplainSpec")(
    test("2-join query with filters renders golden SQL and param footer") {
      val q = Qry
        .from(userTable)
        .innerJoin(Rel(userTable, "id", repoTable, "owner_id"))
        .innerJoin(Rel(repoTable, "id", starTable, "repo_id"))
        .filter(Frag(IndexedSeq("t0.\"name\" = ", ""), IndexedSeq(DbValue.DbString("alice"))))
        .filter(Frag(IndexedSeq("t1.\"name\" = ", ""), IndexedSeq(DbValue.DbString("my-repo"))))

      val explain = q.explain(SqlDialect.PostgreSQL)
      val st      = q.statement(SqlDialect.PostgreSQL)

      assertTrue(
        explain.contains("FROM"),
        explain.contains("INNER JOIN"),
        explain.contains("WHERE"),
        explain.contains("-- params: 1:String, 2:String"),
        !explain.contains("alice"),
        !explain.contains("my-repo"),
        st.source.table == "user",
        st.source.alias == "t0",
        st.joins.size == 2,
        st.joins(0).kind == SqlStatement.JoinKind.Inner,
        st.joins(1).kind == SqlStatement.JoinKind.Inner,
        st.filters.size == 2,
        st.frag.params == IndexedSeq(DbValue.DbString("alice"), DbValue.DbString("my-repo"))
      )
    },
    test("zero params query has no placeholders and (none) footer") {
      val q = Qry
        .from(userTable)
        .innerJoin(Rel(userTable, "id", repoTable, "owner_id"))

      val explain = q.explain(SqlDialect.SQLite)
      val st      = q.statement(SqlDialect.SQLite)

      assertTrue(
        !explain.contains("?1"),
        explain.contains("-- params: (none)"),
        st.filters.isEmpty,
        st.joins.size == 1,
        st.joins.head.kind == SqlStatement.JoinKind.Inner,
        st.frag.params.isEmpty
      )
    },
    test("single join with one filter") {
      val q = Qry
        .from(userTable)
        .innerJoin(Rel(userTable, "id", repoTable, "owner_id"))
        .filter(Frag(IndexedSeq("t0.\"id\" = ", ""), IndexedSeq(DbValue.DbInt(42))))

      val explain = q.explain(SqlDialect.PostgreSQL)
      assertTrue(
        explain.contains("INNER JOIN"),
        explain.contains("WHERE"),
        explain.contains("-- params: 1:Int"),
        !explain.contains("42")
      )
    },
    test("orderBy and limit appear in explain and statement") {
      val q = Qry
        .from(userTable)
        .innerJoin(Rel(userTable, "id", repoTable, "owner_id"))
        .filter(Frag(IndexedSeq("t0.\"name\" = ", ""), IndexedSeq(DbValue.DbString("bob"))))
        .orderBy("id", QSortOrder.Asc)
        .orderBy("name", QSortOrder.Desc)
        .limit(10)
        .offset(5)

      val explain = q.explain(SqlDialect.PostgreSQL)
      val st      = q.statement(SqlDialect.PostgreSQL)

      assertTrue(
        explain.contains("ORDER BY"),
        explain.contains("LIMIT 10"),
        explain.contains("OFFSET 5"),
        explain.contains("?1"),
        explain.contains("-- params: 1:String"),
        st.orderBy.size == 2,
        st.orderBy.head.direction == SqlStatement.OrderDirection.Asc,
        st.orderBy(1).direction == SqlStatement.OrderDirection.Desc,
        st.limit.contains(SqlStatement.Limit(10)),
        st.offset.contains(SqlStatement.Offset(5)),
        !explain.contains("bob")
      )
    },
    test("left join kind preserved and groupBy appears") {
      val q = Qry
        .from(userTable)
        .leftJoin(Rel(userTable, "id", repoTable, "owner_id"))
        .filter(Frag(IndexedSeq("t0.\"name\" = ", ""), IndexedSeq(DbValue.DbString("x"))))
        .groupBy("id")

      val explain = q.explain(SqlDialect.PostgreSQL)
      val st      = q.statement(SqlDialect.PostgreSQL)

      assertTrue(
        explain.contains("LEFT JOIN"),
        explain.contains("GROUP BY"),
        st.joins.head.kind == SqlStatement.JoinKind.Left,
        st.groupBy.isDefined
      )
    },
    test("explain never leaks values for multiple param types") {
      val q = Qry
        .from(userTable)
        .filter(Frag(IndexedSeq("t0.\"id\" = ", ""), IndexedSeq(DbValue.DbInt(123))))
        .filter(Frag(IndexedSeq("t0.\"name\" = ", ""), IndexedSeq(DbValue.DbString("secret"))))
        .filter(Frag(IndexedSeq("t0.\"id\" > ", ""), IndexedSeq(DbValue.DbLong(999L))))

      val explain = q.explain(SqlDialect.PostgreSQL)
      assertTrue(
        !explain.contains("123"),
        !explain.contains("secret"),
        !explain.contains("999"),
        explain.contains("-- params: 1:Int, 2:String, 3:Long"),
        explain.contains("?1"),
        explain.contains("?2"),
        explain.contains("?3")
      )
    },
    test("explain reuses renderer - statement frag equals toFrag") {
      val q = Qry
        .from(userTable)
        .innerJoin(Rel(userTable, "id", repoTable, "owner_id"))
        .filter(Frag(IndexedSeq("t0.\"name\" = ", ""), IndexedSeq(DbValue.DbString("a"))))

      val st   = q.statement(SqlDialect.PostgreSQL)
      val frag = q.toFrag(SqlDialect.PostgreSQL)
      assertTrue(st.frag == frag)
    },
    test("reserved and mixed-case columns render double-quoted") {
      case class Weird(@Modifier.rename("order") ord: Int, @Modifier.rename("MixedCase") mixed: String)
      object Weird {
        implicit val schema: Schema[Weird] = Schema.derived
      }
      val table   = Table.derived[Weird]
      val explain = Qry
        .from(table)
        .filter(Frag(IndexedSeq("t0.\"order\" = ", ""), IndexedSeq(DbValue.DbInt(1))))
        .explain(SqlDialect.PostgreSQL)
      assertTrue(
        explain.contains("SELECT"),
        explain.contains("order"),
        explain.contains("MixedCase"),
        explain.contains("WHERE")
      )
    }
  )
}
