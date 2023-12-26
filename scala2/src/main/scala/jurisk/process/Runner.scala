package jurisk.process

import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.io.Source
import scala.sys.process.ProcessIO
import scala.sys.process.stringSeqToProcess

object Runner {
  def runSync(command: String*)(input: String): String = {
    val outputBuilder = new StringBuilder
    val io            = new ProcessIO(
      in => {
        in.write(input.getBytes)
        in.close()
      },
      out => {
        val output = Source.fromInputStream(out).mkString
        outputBuilder.append(output)
        out.close()
      },
      _.close(),
    )
    val process       = command.run(io)
    process.exitValue() shouldEqual 0
    outputBuilder.toString()
  }
}
