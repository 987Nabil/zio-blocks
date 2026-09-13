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

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import zio.test.*
import zio.blocks.schema.Schema
import zio.blocks.sql.query.{SortOrder => QSortOrder, SqlQuery => Qry, Rel}

private object IrFullFixture {
  case class User(id: Int, name: String)
  object User { implicit val schema: Schema[User] = Schema.derived }
  case class Repo(id: Int, ownerId: Int, name: String)
  object Repo { implicit val schema: Schema[Repo] = Schema.derived }
  case class Star(userId: Int, repoId: Int)
  object Star { implicit val schema: Schema[Star] = Schema.derived }
  val userTable               = Table.derived[User]
  val repoTable               = Table.derived[Repo]
  val starTable               = Table.derived[Star]
  inline def query: Qry[User] =
    Qry
      .from(userTable)
      .innerJoin(Rel(repoTable, "owner_id", userTable, "id"))
      .innerJoin(Rel(starTable, "repo_id", repoTable, "id"))
      .filter(Frag(IndexedSeq("t0.\"name\" = ", ""), IndexedSeq(DbValue.DbString("alice"))))
      .groupBy("name")
      .orderBy("name", QSortOrder.Asc)
      .limit(10)
      .offset(5)
  Dump.dumpQuery(query)
}

object ExplainDumpGoldenSpec extends ZIOSpecDefault {

  case class User(id: Int, name: String)
  object User { implicit val schema: Schema[User] = Schema.derived }
  case class Repo(id: Int, ownerId: Int, name: String)
  object Repo { implicit val schema: Schema[Repo] = Schema.derived }
  case class Star(userId: Int, repoId: Int)
  object Star { implicit val schema: Schema[Star] = Schema.derived }
  val userTable = Table.derived[User]
  val repoTable = Table.derived[Repo]
  val starTable = Table.derived[Star]

  private def normalizeExplainBody(explain: String): String = {
    val body = explain.split("\n-- params:").head.trim
    normalizeSql(body)
  }
  private def normalizeSql(sql: String): String = {
    val noQuotes = sql.replace("\"", "")
    val noAs     = noQuotes.replaceAll("(?i)\\s+AS\\s+", " ")
    val qNorm    = noAs.replaceAll("\\?[0-9]+", "?")
    qNorm.replaceAll("\\s+", " ").trim
  }
  private def dumpDirOpt: Option[Path] =
    Option(System.getProperty("zib.sql.dumpDir")).map(Paths.get(_))

  private def findDumpContaining(fragment: String): Option[String] =
    dumpDirOpt.flatMap { dir =>
      if (!Files.exists(dir)) None
      else {
        val stream = Files.list(dir)
        try {
          val it                    = stream.iterator()
          var found: Option[String] = None
          while (it.hasNext && found.isEmpty) {
            val p = it.next()
            if (p.toString.endsWith(".sql")) {
              val content = new String(Files.readAllBytes(p), StandardCharsets.UTF_8)
              if (normalizeSql(content).contains(normalizeSql(fragment))) found = Some(content)
            }
          }
          found
        } finally stream.close()
      }
    }

  def spec = suite("ExplainDumpGoldenSpec")(
    test("IR 2-join with filters, groupBy, orderBy, limit/offset via dumpQuery equals runtime sql normalized") {
      val q      = IrFullFixture.query
      val fragPg = q.toFrag(SqlDialect.PostgreSQL).sql(SqlDialect.PostgreSQL)
      dumpDirOpt match {
        case None =>
          assertTrue(true) // skipped — run with -Dzib.sql.dumpDir=target/sql-dumps
        case Some(_) =>
          val expected = normalizeSql(fragPg)
          val found    = findDumpContaining(expected)
          assertTrue(found.isDefined) &&
          assertTrue(
            normalizeSql(found.get) == expected,
            found.get.contains("INNER JOIN") || normalizeSql(found.get).contains("INNER JOIN"),
            found.get.contains("WHERE") || normalizeSql(found.get).contains("WHERE"),
            found.get.contains("GROUP BY") || normalizeSql(found.get).contains("GROUP BY"),
            found.get.contains("ORDER BY") || normalizeSql(found.get).contains("ORDER BY"),
            found.get.contains("LIMIT 10"),
            found.get.contains("OFFSET 5"),
            !found.get.contains("alice")
          )
      }
    },
    test("IR query explain produces correct output") {
      val q       = IrFullFixture.query
      val explain = q.explain(SqlDialect.PostgreSQL)
      assertTrue(
        explain.contains("FROM"),
        explain.contains("INNER JOIN"),
        explain.contains("WHERE"),
        explain.contains("GROUP BY"),
        explain.contains("ORDER BY"),
        explain.contains("LIMIT 10"),
        explain.contains("OFFSET 5"),
        explain.contains("-- params: 1:String"),
        !explain.contains("alice")
      )
    }
  )
}
