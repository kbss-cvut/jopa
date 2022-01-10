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
package cz.cvut.kbss.jopa.test.environment;

import java.util.Map;

/**
 * Persistent storage configuration.
 */
public abstract class StorageConfig {

    protected String name;
    protected String directory;

    public void setName(String name) {
        assert name != null;
        this.name = name;
    }

    public void setDirectory(String directory) {
        assert directory != null;
        this.directory = directory;
    }

    /**
     * Creates ontology storage properties for this storage configuration.
     * <p>
     * This method performs any necessary tasks before creating the storage properties (typically deleting the old
     * data).
     *
     * @param index Index appended to file/folder name, used when multiple storages of the same type can occur
     * @return OntologyStorageProperties
     */
    public abstract Map<String, String> createStorageConfiguration(int index);
}
