### java.lang.IllegalArgumentException: Missing scheme

Uri: <WORKSPACE>/funsets/src/main/scala/funsets/FunSetsInterface.scala


#### Error stacktrace:

```
java.base/java.nio.file.Path.of(Path.java:199)
	java.base/java.nio.file.Paths.get(Paths.java:98)
	scala.meta.internal.mtags.MtagsEnrichments$XtensionURIMtags.toAbsolutePath(MtagsEnrichments.scala:130)
	scala.meta.internal.mtags.MtagsEnrichments$XtensionStringMtags.toAbsolutePath(MtagsEnrichments.scala:187)
	scala.meta.internal.metals.MetalsEnrichments$XtensionString.toAbsolutePath(MetalsEnrichments.scala:773)
	scala.meta.internal.metals.MetalsEnrichments$XtensionString.toAbsolutePath(MetalsEnrichments.scala:770)
	scala.meta.internal.metals.MetalsEnrichments$XtensionString.toAbsolutePathSafe(MetalsEnrichments.scala:756)
	scala.meta.internal.metals.MetalsLspService.$anonfun$reports$2(MetalsLspService.scala:165)
	scala.Option.flatMap(Option.scala:283)
	scala.meta.internal.metals.MetalsLspService.$anonfun$reports$1(MetalsLspService.scala:163)
	scala.meta.internal.metals.StdReporter.reportPath(ReportContext.scala:163)
	scala.meta.internal.metals.StdReporter.create(ReportContext.scala:131)
	scala.meta.internal.metals.ReferenceProvider.$anonfun$references$3(ReferenceProvider.scala:237)
	scala.concurrent.impl.Promise$Transformation.run(Promise.scala:467)
	java.base/java.util.concurrent.ThreadPoolExecutor.runWorker(ThreadPoolExecutor.java:1136)
	java.base/java.util.concurrent.ThreadPoolExecutor$Worker.run(ThreadPoolExecutor.java:635)
	java.base/java.lang.Thread.run(Thread.java:833)
```
#### Short summary: 

java.lang.IllegalArgumentException: Missing scheme