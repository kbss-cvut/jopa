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
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

import java.util.Collection;
import java.util.HashSet;
import java.util.function.Consumer;

public class SimpleOneLevelCascadeExplorer extends OneLevelCascadeExplorer {

    private final Consumer<Object> cascadedOperation;

    protected SimpleOneLevelCascadeExplorer(Consumer<Object> cascadedOperation) {
        this.cascadedOperation = cascadedOperation;
    }

    protected void runForEach(final Attribute<?, ?> at, final Object o, boolean cascaded) {
        Object attVal = EntityPropertiesUtils.getAttributeValue(at, o);
        if (attVal == null) {
            return;
        }
        if (at.isCollection()) {
            for (final Object ox2 : new HashSet<>((Collection<?>) attVal)) {
                if (cascaded) {
                    runCascadedForEach(ox2);
                } else {
                    runNonCascadedForEach(ox2);
                }
            }
        } else {
            if (cascaded) {
                runCascadedForEach(attVal);
            } else {
                runNonCascadedForEach(attVal);
            }
        }
    }

    @Override
    protected void exploreCascaded(final Attribute<?, ?> at, final Object o) {
        runForEach(at, o, true);
    }

    protected void runCascadedForEach(Object ox2) {
        cascadedOperation.accept(ox2);
    }

    @Override
    protected void exploreNonCascaded(final Attribute<?, ?> at, final Object o) {
        runForEach(at, o, false);
    }

    protected void runNonCascadedForEach(Object ox2) {
        // nothing
    }
}
