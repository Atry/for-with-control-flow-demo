trait Mappable[Keyword, +Value]

def[Upstream, Value, Mapped, MappedValue](
  upstream: Upstream
)flatMap(
  using Mappable[Upstream, Value]
)(
  flatMapper: Value => Mapped
)(
  using Mappable[Mapped, MappedValue]
) : keywords.FlatMap[Upstream, Value, Mapped] =
  keywords.FlatMap(upstream, flatMapper)

def[Upstream, Value, Mapped, MappedValue](
  upstream: Upstream
)map(
  using Mappable[Upstream, Value]
)(
  mapper: Value => Mapped
)(
  implicit upstreamMappable: Mappable[Upstream, Value]
) : keywords.Map[Upstream, Value, Mapped] =
  keywords.Map(upstream, mapper)

object keywords {
  case class IfThenElse[If, Then, Else](ifTree: If, thenTree: Then, elseTree: Else)
  export IfThenElse.{apply => ifThenElse}
  given[If, Then, Else, Value](using Mappable[Then, Value])(using Mappable[Else, Value]) as Mappable[IfThenElse[If, Then, Else], Value]

  case class Pure[A](a: () => A)
  def pure[A](a: A) = Pure(() => a)
  given[A] as Mappable[Pure[A], A]

  final case class FlatMap[Upstream, UpstreamValue, Mapped](
    upstream: Upstream,
    flatMapper: UpstreamValue => Mapped
  )
  given[Upstream, UpstreamValue, Mapped, Value](using Mappable[Mapped, Value]) as Mappable[FlatMap[Upstream, UpstreamValue, Mapped], Value]
  final case class Map[Upstream, UpstreamValue, Mapped](
    upstream: Upstream,
    mapper: UpstreamValue => Mapped
  )
  given[Upstream, UpstreamValue, Mapped] as Mappable[Map[Upstream, UpstreamValue, Mapped], Mapped]

  // Other control flow ASTs
  // ...
}

object Ajax {
  trait Response {
    def responseText: String
  }
  case class get[Url](url: Url)
  given[Url] as Mappable[Ajax.get[Url], Response]
}


import scala.language.dynamics
case class SelectDynamic[Parent, Field <: String & Singleton](parent: Parent, field: Field) extends Dynamic
def [Parent <: Dynamic](parent: Parent)selectDynamic(field: String) = SelectDynamic[Parent, field.type](parent, field)
object JSON {
  case class parse(json: String) extends Dynamic
}

def findWebSite(repositorySlug: String) = {
  for {
    repositoryResponse <- Ajax.get(s"https://api.github.com/repos/$repositorySlug")
    repository = JSON.parse(repositoryResponse.responseText)
    homepage = (repository: JSON.parse).homepage
    webSite <- keywords.ifThenElse(
      keywords.pure(homepage != null),
      keywords.pure {
        homepage.toString()
      },
      for {
        ownerResponse <- Ajax.get(repository.owner.url.toString())
        owner = JSON.parse(ownerResponse.responseText).owner
      } yield owner.blog.toString()
    )
  } yield webSite
}

@main def main = {
  val reified = findWebSite("Atry/Dsl.scala")
  summon[
    reified.type
     <:<
    keywords.FlatMap[
      keywords.Map[Ajax.get[String], Ajax.Response, (Ajax.Response, JSON.parse, 
        SelectDynamic[JSON.parse, "homepage"]
      )]
    , (Ajax.Response, JSON.parse, SelectDynamic[JSON.parse, "homepage"])
      , 
    keywords.Map[
      keywords.IfThenElse[keywords.Pure[Boolean], keywords.Pure[String], 
        keywords.Map[
          keywords.Map[Ajax.get[String], Ajax.Response, (Ajax.Response, 
            SelectDynamic[JSON.parse, "owner"]
          )]
        , (Ajax.Response, SelectDynamic[JSON.parse, "owner"]), String]
      ]
    , String, String]
    ]
  ]
  println(reified)
}
