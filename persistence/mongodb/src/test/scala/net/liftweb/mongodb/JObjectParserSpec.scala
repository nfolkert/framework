/*
 * Copyright 2010-2011 WorldWide Conferencing, LLC
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

package net.liftweb
package mongodb

import java.text.SimpleDateFormat
import java.util.Date
import java.util.regex.Pattern
import java.util.UUID

import org.bson.types.ObjectId
import org.specs.Specification

import net.liftweb.json.Extraction
import net.liftweb.json.JsonAST.JObject

package jobjectparserspec {


case class IntCase(v: Int)
  case class OptIntCase(vOpt: Option[Int])
  case class ListIntCase(vList: List[Int])

  case class OidCase(oid: ObjectId)
  case class OptOidCase(oidOpt: Option[ObjectId])
  case class ListOidCase(oidList: List[ObjectId])

  case class DateCase(date: Date)
  case class OptDateCase(dateOpt: Option[Date])
  case class ListDateCase(dateList: List[Date])

  case class PatternCase(pattern: Pattern)
  case class OptPatternCase(patternOpt: Option[Pattern])
  case class ListPatternCase(patternList: List[Pattern])

  case class UuidCase(uuid: UUID)
  case class OptUuidCase(uuidOpt: Option[UUID])
  case class ListUuidCase(uuidList: List[UUID])

}


/**
 * Systems under specification for CustomSerializers.
 */
object JObjectParserSpec extends Specification("JObjectParser Specification") {

  import jobjectparserspec._

  def fakeOid(x: Int) = new ObjectId(x, x, x)
  def fakeDate(x: Int) = {
    new SimpleDateFormat("MM/dd/yyyy").parse("3/" + x + "/2011")
  }
  def fakePattern(x: Int) = {
    Pattern.compile("" + x)
  }
  def fakeUUID(x: Int) = {
    new UUID(x.toLong, x.toLong)
  }

  implicit val formats = net.liftweb.json.DefaultFormats +
      new ObjectIdSerializer + new UUIDSerializer + new DateSerializer + new PatternSerializer

  "JObjectParser" should {

    "serialize and deserialize case classes with Int field" in {
      val c1 = new IntCase(1);
      val j1 = Extraction.decompose(c1);
      val d1 = JObjectParser.parse(j1.asInstanceOf[JObject]);
      val j2 = JObjectParser.serialize(d1);
      val c2 = j2.extract[IntCase];
      c1 must_== c2
    }

    "serialize and deserialize case classes with Some[Int]" in {
      val c1 = new OptIntCase(Some(2));
      val j1 = Extraction.decompose(c1);
      val d1 = JObjectParser.parse(j1.asInstanceOf[JObject]);
      val j2 = JObjectParser.serialize(d1);
      val c2 = j2.extract[OptIntCase];
      c1 must_== c2
    }

    "serialize and deserialize case classes with None[Int]" in {
      val c1 = new OptIntCase(None);
      val j1 = Extraction.decompose(c1);
      val d1 = JObjectParser.parse(j1.asInstanceOf[JObject]);
      val j2 = JObjectParser.serialize(d1);
      val c2 = j2.extract[OptIntCase];
      c1 must_== c2
    }

    "serialize and deserialize case classes with List[Int]" in {
      val c1 = new ListIntCase(List(3));
      val j1 = Extraction.decompose(c1);
      val d1 = JObjectParser.parse(j1.asInstanceOf[JObject]);
      val j2 = JObjectParser.serialize(d1);
      val c2 = j2.extract[ListIntCase];
      c1 must_== c2
    }

    "serialize and deserialize case classes with Nil[Int]" in {
      val c1 = new ListIntCase(Nil);
      val j1 = Extraction.decompose(c1);
      val d1 = JObjectParser.parse(j1.asInstanceOf[JObject]);
      val j2 = JObjectParser.serialize(d1);
      val c2 = j2.extract[ListIntCase];
      c1 must_== c2
    }

    "serialize and deserialize case classes with ObjectId field" in {
      val c1 = new OidCase(fakeOid(4));
      val j1 = Extraction.decompose(c1);
      val d1 = JObjectParser.parse(j1.asInstanceOf[JObject]);
      val j2 = JObjectParser.serialize(d1);
      val c2 = j2.extract[OidCase];
      c1 must_== c2
    }

    "serialize and deserialize case classes with Some[ObjectId]" in {
      val c1 = new OptOidCase(Some(fakeOid(5)));
      val j1 = Extraction.decompose(c1);
      val d1 = JObjectParser.parse(j1.asInstanceOf[JObject]);
      val j2 = JObjectParser.serialize(d1);
      val c2 = j2.extract[OptOidCase];
      c1 must_== c2
    }

    "serialize and deserialize case classes with None[ObjectId]" in {
      val c1 = new OptOidCase(None);
      val j1 = Extraction.decompose(c1);
      val d1 = JObjectParser.parse(j1.asInstanceOf[JObject]);
      val j2 = JObjectParser.serialize(d1);
      val c2 = j2.extract[OptOidCase];
      c1 must_== c2
    }

    "serialize and deserialize case classes with List[ObjectId]" in {
      val c1 = new ListOidCase(List(fakeOid(6)));
      val j1 = Extraction.decompose(c1);
      val d1 = JObjectParser.parse(j1.asInstanceOf[JObject]);
      val j2 = JObjectParser.serialize(d1);
      val c2 = j2.extract[ListOidCase];
      c1 must_== c2
    }

    "serialize and deserialize case classes with Nil[ObjectId]" in {
      val c1 = new ListOidCase(Nil);
      val j1 = Extraction.decompose(c1);
      val d1 = JObjectParser.parse(j1.asInstanceOf[JObject]);
      val j2 = JObjectParser.serialize(d1);
      val c2 = j2.extract[ListOidCase];
      c1 must_== c2
    }

    "serialize and deserialize case classes with Date field" in {
      val c1 = new DateCase(fakeDate(7));
      val j1 = Extraction.decompose(c1);
      val d1 = JObjectParser.parse(j1.asInstanceOf[JObject]);
      val j2 = JObjectParser.serialize(d1);
      val c2 = j2.extract[DateCase];
      c1 must_== c2
    }

    "serialize and deserialize case classes with Some[Date]" in {
      val c1 = new OptDateCase(Some(fakeDate(8)));
      val j1 = Extraction.decompose(c1);
      val d1 = JObjectParser.parse(j1.asInstanceOf[JObject]);
      val j2 = JObjectParser.serialize(d1);
      val c2 = j2.extract[OptDateCase];
      c1 must_== c2
    }

    "serialize and deserialize case classes with List[Date]" in {
      val c1 = new ListDateCase(List(fakeDate(9)));
      val j1 = Extraction.decompose(c1);
      val d1 = JObjectParser.parse(j1.asInstanceOf[JObject]);
      val j2 = JObjectParser.serialize(d1);
      val c2 = j2.extract[ListDateCase];
      c1 must_== c2
    }

    // Patterns are compared as strings
    "serialize and deserialize case classes with Pattern field" in {
      val c1 = new PatternCase(fakePattern(10));
      val j1 = Extraction.decompose(c1);
      val d1 = JObjectParser.parse(j1.asInstanceOf[JObject]);
      val j2 = JObjectParser.serialize(d1);
      val c2 = j2.extract[PatternCase];
      c1.toString must_== c2.toString
    }

    "serialize and deserialize case classes with Some[Pattern]" in {
      val c1 = new OptPatternCase(Some(fakePattern(11)));
      val j1 = Extraction.decompose(c1);
      val d1 = JObjectParser.parse(j1.asInstanceOf[JObject]);
      val j2 = JObjectParser.serialize(d1);
      val c2 = j2.extract[OptPatternCase];
      c1.toString must_== c2.toString
    }

    "serialize and deserialize case classes with List[Pattern]" in {
      val c1 = new ListPatternCase(List(fakePattern(12)));
      val j1 = Extraction.decompose(c1);
      val d1 = JObjectParser.parse(j1.asInstanceOf[JObject]);
      val j2 = JObjectParser.serialize(d1);
      val c2 = j2.extract[ListPatternCase];
      c1.toString must_== c2.toString
    }

    "serialize and deserialize case classes with UUID field" in {
      val c1 = new UuidCase(fakeUUID(13));
      val j1 = Extraction.decompose(c1);
      val d1 = JObjectParser.parse(j1.asInstanceOf[JObject]);
      val j2 = JObjectParser.serialize(d1);
      val c2 = j2.extract[UuidCase];
      c1 must_== c2
    }

    "serialize and deserialize case classes with Some[UUID]" in {
      val c1 = new OptUuidCase(Some(fakeUUID(14)));
      val j1 = Extraction.decompose(c1);
      val d1 = JObjectParser.parse(j1.asInstanceOf[JObject]);
      val j2 = JObjectParser.serialize(d1);
      val c2 = j2.extract[OptUuidCase];
      c1 must_== c2
    }

    "serialize and deserialize case classes with List[UUID]" in {
      val c1 = new ListUuidCase(List(fakeUUID(15)));
      val j1 = Extraction.decompose(c1);
      val d1 = JObjectParser.parse(j1.asInstanceOf[JObject]);
      val j2 = JObjectParser.serialize(d1);
      val c2 = j2.extract[ListUuidCase];
      c1 must_== c2
    }
  }
}
