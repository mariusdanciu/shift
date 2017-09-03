package net.shift.server

package object http {

  type ResponseFunc = Response => ResponseFeedback

  implicit def enrichResp(r: Response): RichResponse = new RichResponse(r)

  implicit def enrichReq(r: Request): RichRequest = new RichRequest(r)

}

sealed trait ResponseFeedback

case object Committed extends ResponseFeedback

case object AlreadyCommitted extends ResponseFeedback

case class CommitFailure(t: Throwable) extends ResponseFeedback

