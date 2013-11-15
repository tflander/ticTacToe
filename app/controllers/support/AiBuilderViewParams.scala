package controllers.support

case class AiBuilderViewParams (
		sampleRules: Seq[String], 
		usage: String, 
		xRule: String = "", 
		oRule: String = "", 
		xResult: (Boolean, String) = (true, ""), 
		oResult: (Boolean, String) = (true, ""),
		matchResults: String = ""
)