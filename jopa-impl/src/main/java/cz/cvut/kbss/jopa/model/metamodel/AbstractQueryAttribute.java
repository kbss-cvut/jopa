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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.jopa.oom.converter.ConverterWrapper;

import java.lang.reflect.Field;
import java.lang.reflect.Member;

/**
 * A common class to all attributes defined by a query
 *
 * @param <X> The represented type that contains the attribute
 * @param <Y> The type of the represented attribute
 */
public abstract class AbstractQueryAttribute<X, Y> implements QueryAttribute<X, Y> {

    /**
     * Name of the variable which may be used in the query and would be replaced by the identifier of the entity owning
     * this attribute.
     */
    public static final String THIS_PARAMETER = "this";

    private final String query;

    private final boolean enableReferencingAttributes;

    private final Field field;

    private final ManagedType<X> declaringType;

    private final FetchType fetchType;

    private final ParticipationConstraint[] constraints;

    private final ConverterWrapper converter;

    public AbstractQueryAttribute(String query, boolean enableReferencingAttributes, Field field,
                                  ManagedType<X> declaringType, FetchType fetchType,
                                  ParticipationConstraint[] constraints, ConverterWrapper converter) {
        this.query = query;
        this.enableReferencingAttributes = enableReferencingAttributes;
        this.field = field;
        this.declaringType = declaringType;
        this.fetchType = fetchType;
        this.constraints = constraints;
        this.converter = converter;
    }

    @Override
    public String getQuery() {
        return query;
    }

    @Override
    public boolean enableReferencingAttributes() {
        return enableReferencingAttributes;
    }

    @Override
    public Member getJavaMember() {
        return field;
    }

    @Override
    public ParticipationConstraint[] getConstraints() {
        return constraints;
    }

    @Override
    public ManagedType<X> getDeclaringType() {
        return declaringType;
    }

    @Override
    public Field getJavaField() {
        return field;
    }

    @Override
    public FetchType getFetchType() {
        return fetchType;
    }

    /**
     * A query based attribute is always inferred.
     *
     * @return always {@code true}
     */
    @Override
    public boolean isInferred() {
        return true;
    }

    /**
     * A query based attribute always includes explicit.
     *
     * @return always {@code true}
     */
    @Override
    public boolean includeExplicit() {
        return true;
    }

    @Override
    public String getName() {
        return field.getName();
    }

    @Override
    public abstract boolean isCollection();

    public ConverterWrapper getConverter() {
        return converter;
    }

    @Override
    public String toString() {
        return declaringType.getJavaType().getSimpleName() + "." + getName();
    }
}
