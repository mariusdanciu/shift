package net.shift {
package http {


trait PageRule

class PreProc(f: PartialFunction[Request, Response]) extends PageRule

class PostProc(f: PartialFunction[Response, Response]) extends PageRule

}
}
