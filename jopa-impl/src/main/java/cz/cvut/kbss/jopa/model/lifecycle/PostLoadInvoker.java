/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model.lifecycle;

import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.metamodel.EntityTypeImpl;

import java.util.function.Consumer;

/**
 * Invokes post load entity listeners for the passed object.
 */
public class PostLoadInvoker implements Consumer<Object> {

    private final MetamodelImpl metamodel;

    public PostLoadInvoker(MetamodelImpl metamodel) {
        this.metamodel = metamodel;
    }

    @Override
    public void accept(Object o) {
        final EntityTypeImpl<?> et = metamodel.entity(o.getClass());
        et.getLifecycleListenerManager().invokePostLoadCallbacks(o);
    }
}
