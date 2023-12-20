/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
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
