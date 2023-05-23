package cz.cvut.kbss.jopa.maven;

import org.apache.maven.plugin.logging.Log;

class Utils {

    private Utils() {
        throw new AssertionError();
    }

    static void logParameterValue(String param, Object value, Log logger) {
        logger.info(param + ": " + value);
    }
}
