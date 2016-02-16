/**
 * Copyright (C) 2011 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;

public interface MetamodelProvider {

    /**
     * Gets the metamodel
     * @return Metamodel
     */
    Metamodel getMetamodel();

    /**
     * Checks whether the specified class is a managed (=entity) type.
     * @param cls The class to check
     */
    boolean isTypeManaged(Class<?> cls);
}
