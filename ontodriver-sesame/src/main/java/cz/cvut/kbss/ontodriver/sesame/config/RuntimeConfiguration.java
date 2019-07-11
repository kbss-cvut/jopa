package cz.cvut.kbss.ontodriver.sesame.config;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;

/**
 * Represents configuration which influences the driver during its active usage, not its initialization.
 */
public class RuntimeConfiguration {

    private final int loadAllThreshold;

    public RuntimeConfiguration(DriverConfiguration config) {
        if (config.isSet(SesameConfigParam.LOAD_ALL_THRESHOLD)) {
            try {
                this.loadAllThreshold = Integer.parseInt(config.getProperty(SesameConfigParam.LOAD_ALL_THRESHOLD));
            } catch (NumberFormatException e) {
                throw new IllegalArgumentException(
                        "Invalid value of the \"" + SesameOntoDriverProperties.SESAME_LOAD_ALL_THRESHOLD +
                                "\" parameter. Must be a valid integer.", e);
            }
        } else {
            this.loadAllThreshold = Constants.DEFAULT_LOAD_ALL_THRESHOLD;
        }
    }

    public int getLoadAllThreshold() {
        return loadAllThreshold;
    }
}
