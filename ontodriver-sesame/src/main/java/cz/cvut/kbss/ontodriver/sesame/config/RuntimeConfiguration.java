/**
 * Copyright (C) 2022 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
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
