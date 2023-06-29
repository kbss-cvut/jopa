/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.rdf4j.config;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.rdf4j.loader.DefaultStatementLoaderFactory;
import cz.cvut.kbss.ontodriver.rdf4j.loader.StatementLoaderFactory;

/**
 * Represents configuration which influences the driver during its active usage, not its initialization.
 */
public class RuntimeConfiguration {

    private final int loadAllThreshold;

    private StatementLoaderFactory statementLoaderFactory = new DefaultStatementLoaderFactory();

    public RuntimeConfiguration(DriverConfiguration config) {
        if (config.isSet(Rdf4jConfigParam.LOAD_ALL_THRESHOLD)) {
            try {
                this.loadAllThreshold = Integer.parseInt(config.getProperty(Rdf4jConfigParam.LOAD_ALL_THRESHOLD));
            } catch (NumberFormatException e) {
                throw new IllegalArgumentException(
                        "Invalid value of the \"" + Rdf4jOntoDriverProperties.LOAD_ALL_THRESHOLD +
                                "\" parameter. Must be a valid integer.", e);
            }
        } else {
            this.loadAllThreshold = Constants.DEFAULT_LOAD_ALL_THRESHOLD;
        }
    }

    public int getLoadAllThreshold() {
        return loadAllThreshold;
    }

    public StatementLoaderFactory getStatementLoaderFactory() {
        return statementLoaderFactory;
    }

    public void setStatementLoaderFactory(StatementLoaderFactory statementLoaderFactory) {
        assert statementLoaderFactory != null;
        this.statementLoaderFactory = statementLoaderFactory;
    }
}
