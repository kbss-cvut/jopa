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
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.proxy.lazy.LazyLoadingProxy;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.jopa.utils.JOPALazyUtils;

import java.util.Collection;
import java.util.HashSet;
import java.util.function.Consumer;

public class OneLevelRemoveCascadeExplorer extends OneLevelCascadeExplorer {

    private final Consumer<Object> removeOperation;

    public OneLevelRemoveCascadeExplorer(Consumer<Object> removeOperation) {
        this.removeOperation = removeOperation;
    }

    @Override
    protected void exploreCascaded(Attribute<?, ?> at, Object o) {
        Object attVal = EntityPropertiesUtils.getAttributeValue(at, o);
        if (attVal == null) {
            return;
        }
        if (JOPALazyUtils.isLazyLoadingProxy(attVal)) {
            attVal = ((LazyLoadingProxy<?>) attVal).triggerLazyLoading();
        }
        if (at.isCollection()) {
            for (final Object ox2 : new HashSet<>((Collection<?>) attVal)) {
                removeOperation.accept(ox2);
            }
        } else {
            removeOperation.accept(attVal);
        }
    }
}
