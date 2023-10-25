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
package cz.cvut.kbss.jopa.oom.query;

import cz.cvut.kbss.jopa.model.metamodel.AbstractQueryAttribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.SingularQueryAttribute;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.oom.converter.ConverterWrapper;
import cz.cvut.kbss.jopa.oom.converter.DefaultConverterWrapper;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

/**
 * @param <T> The query attribute type, e.g. {@link SingularQueryAttribute}
 * @param <X> Entity class
 */
public abstract class QueryFieldStrategy<T extends AbstractQueryAttribute<? super X, ?>, X> {

    final EntityType<X> et;
    final T attribute;

    private final ConverterWrapper<Object, Object> converter;

    protected QueryFieldStrategy(EntityType<X> et, T attribute) {
        this.et = et;
        this.attribute = attribute;
        this.converter = attribute.getConverter() != null ? attribute.getConverter() : DefaultConverterWrapper.INSTANCE;
    }

    /**
     * Adds value from the specified typed query to this strategy.
     * <p>
     * The value(s) is/are then set on entity field using {@link #buildInstanceFieldValue(Object)}.
     *
     * @param typedQuery typed query to extract value from
     */
    public abstract void addValueFromTypedQuery(TypedQuery<?> typedQuery);

    /**
     * Sets instance field from values gathered in this strategy.
     *
     * @param instance The instance to receive the field value
     */
    public abstract void buildInstanceFieldValue(Object instance);

    /**
     * Sets the specified value on the specified instance, the field is taken from the attribute represented by this
     * strategy.
     * <p>
     * Note that this method assumes the value and the field are of compatible types, no check is done here.
     */
    void setValueOnInstance(Object instance, Object value) {
        EntityPropertiesUtils.setFieldValue(attribute.getJavaField(), instance, value);
    }

    boolean isValidRange(Object value) {
        return attribute.getJavaType().isAssignableFrom(value.getClass()) || canBeConverted(value);
    }

    boolean canBeConverted(Object value) {
        return converter.supportsAxiomValueType(value.getClass());
    }

    Object toAttributeValue(Object value) {
        return converter.convertToAttribute(value);
    }
}
