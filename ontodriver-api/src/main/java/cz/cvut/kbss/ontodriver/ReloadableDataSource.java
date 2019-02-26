/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

/**
 * Allows to reload data from the underlying storage.
 * <p>
 * This is especially meant for reloading data from text files (e.g. OWL, TTL), which might have been changed externally
 * and the application may want to reload the file contents so that it works with the most recent data.
 */
public interface ReloadableDataSource extends DataSource {

    /**
     * Reloads data from the underlying storage, if possible.
     * <p>
     * Does nothing if the underlying storage does not support reloading or it is not necessary (e.g. when connected
     * to a remote triple store).
     *
     * @throws OntoDriverException   If an error occurs when reloading data
     * @throws IllegalStateException If called on a closed data source
     */
    void reload() throws OntoDriverException;
}
