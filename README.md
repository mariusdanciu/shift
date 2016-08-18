Shift is a pure stateless and asynchronous web toolkit written in Scala, being inspired by LiftWeb.

Briefly:

- Uses state monads to process requests, specify routes etc.
- Uses state monads to propagate state between snippets in html templates.
- Lightweight template engine that supports merging templates leading to vast reuse of common templates
- Responses are async by design

# Shift Templates

## Snippets
   
   A snippet is a function that process a page fragment. Thus it is mostly used to create server side dynamic content.
   
### Snippet definition inside the HTML page
For exemplification let's say that we want to render a set of images. Obviously from the HTML page level we don't know what those images are as their URL will have to be computed by the server. But we do know the layout on how we want to render an image. For this we can have the following code:

```html
   <span>Content before</span>
   <ul>
   <!-- snip:mysnippet -->
     <li class="product">
       <img src=""/>
     </li>
   <!-- end -->
   </ul>
   <span>Content after</span>
```   
The page fragment between the two comments is the page fragment that will be passed to the Scala snippet as a NodeSeq. Thus it has to be XML well formed.

### Scala snippet definition
   The Scala snippets for a page are specified as part of the net.shift.template.DynamicContent trait.
   
```scala
  import net.shift.common.XmlImplicits._
  import net.shift.common.XmlUtils._
  import net.shift.common.Xml
  import Binds._
  import Template._

  val snippets = new DynamicContent[String] {

    def snippets = List(
      snip("mysnippet") {
        s =>
          val imageURLs = getImageURLsFromSomewhere()
          imageURLs map { url => 
            bind (s.node) {
              case Xml("img", attrs, childs) => Xml("img") % (attrs + ("src", url))
            }
          }
          
          Success(("form", <div id="processed">{ s.node }</div>))
      },
  }

```
Here we used the bind API in order to manipulate the HTML fragment (s.node). Here we are just looking for the img node and append the src attribute. You can manipulate s.node in any other way of course.

Manually running this template can be done with :

```scala

  val r:  Try[(PageState[String], String)] = Template().run(page, snippets, PageState("", Language("en"), None))

```
where:
- page is the Html page as a String
- snippets is the DynamicContent instance
- PageState is the initial state provided to the template engine. Internally snippets are really state monads and the state is propagated from one snippet to the next one.
- the result is a pair of output state and rendered content.




## Inlines
## Templates reference
## Localization
