/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.jopa.oom.converter.ConverterWrapper;

import java.lang.reflect.Field;

/**
 * Singular query attributes contain a single value or reference, ie. they are not collections.
 *
 * @param <X> The represented type that contains the attribute
 * @param <Y> The type of the represented attribute
 */
public class SingularQueryAttributeImpl<X, Y> extends AbstractQueryAttribute<X, Y> implements SingularQueryAttribute<X, Y> {

    private final Type<Y> type;

    public SingularQueryAttributeImpl(String query, boolean enableReferencingAttributes, Field field,
                                      ManagedType<X> declaringType, FetchType fetchType,
                                      Type<Y> type, ParticipationConstraint[] constraints, ConverterWrapper converter) {
        super(query, enableReferencingAttributes, field, declaringType, fetchType, constraints, converter);
        this.type = type;
    }

    @Override
    public Type<Y> getType() {
        return type;
    }

    @Override
    public Class<Y> getJavaType() {
        return type.getJavaType();
    }

    @Override
    public boolean isCollection() {
        return false;
    }
}
