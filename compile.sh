mkdir -p classes
clojure -M -e "(compile 'cr2.core)"
clojure -M:uberjar --main-class cr2.core
native-image -jar target/cr2.0-calculator.jar \
        --no-fallback \
        --report-unsupported-elements-at-runtime \
        -H:IncludeResourceBundles=consoleui_messages,jline.console.completer.CandidateListCompletionHandler \
        -H:ReflectionConfigurationFiles=graal/reflect-config.json \
        -H:+ReportExceptionStackTraces
