mkdir -p classes
clojure -M -e "(compile 'cr2.core)"
clojure -M:uberjar --main-class cr2.core
native-image -jar target/cr2.0-encounter-calculator.jar \
        -o target/cr2.0-encounter-calculator \
        --no-fallback \
        --report-unsupported-elements-at-runtime \
        -H:IncludeResourceBundles=consoleui_messages,jline.console.completer.CandidateListCompletionHandler \
        -H:ReflectionConfigurationFiles=graal/reflect-config-unix.json \
        -H:ResourceConfigurationFiles=graal/resources-config.json \
        -H:JNIConfigurationFiles=graal/jni-config.json \
        -H:+ReportExceptionStackTraces
