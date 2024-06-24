/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.query.NamedQueryManager;
import cz.cvut.kbss.jopa.query.ResultSetMappingManager;

public interface MetamodelProvider {

    /**
     * Gets the metamodel
     *
     * @return Metamodel
     */
    MetamodelImpl getMetamodel();

    /**
     * Checks whether the specified class is an entity type.
     *
     * @param cls The class to check
     * @return Whether type is managed
     */
    boolean isEntityType(Class<?> cls);

    /**
     * Gets {@link NamedQueryManager} for this persistence unit.
     *
     * @return {@code NamedQueryManager}
     */
    default NamedQueryManager getNamedQueryManager() {
        return getMetamodel().getNamedQueryManager();
    }

    /**
     * Gets the SPARQL result set mapping manager ({@link ResultSetMappingManager}) for this persistence unit.
     *
     * @return {@code ResultSetMappingManager}
     */
    default ResultSetMappingManager getResultSetMappingManager() {
        return getMetamodel().getResultSetMappingManager();
    }
}
