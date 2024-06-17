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
package cz.cvut.kbss.jopa.sessions.change;

import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;

/**
 * Record of a single change to an attribute.
 */
public class ChangeRecord {

    private final FieldSpecification<?, ?> attribute;

    private Object newValue;

    private boolean preventsCaching;

    public ChangeRecord(FieldSpecification<?, ?> att, Object value) {
        assert att != null;
        this.attribute = att;
        this.newValue = value;
    }

    /**
     * Returns the new value of the attribute.
     *
     * @return Object
     */
    public Object getNewValue() {
        return newValue;
    }

    /**
     * Sets the new value of the attribute in case this change record needs to be updated.
     *
     * @param value The value to set
     */
    public void setNewValue(Object value) {
        this.newValue = value;
    }

    /**
     * Gets the attribute to which this change record is bound.
     *
     * @return the attribute
     */
    public FieldSpecification<?, ?> getAttribute() {
        return attribute;
    }

    /**
     * Marks this change record to prevent caching.
     *
     * @see #doesPreventCaching()
     */
    public void preventCaching() {
        this.preventsCaching = true;
    }

    /**
     * Whether this change record prevents caching of the instance on which the change is applied.
     *
     * @return Whether this change record prevents caching
     */
    public boolean doesPreventCaching() {
        return preventsCaching;
    }

    @Override
    public String toString() {
        return "ChangeRecord{" +
                "attribute='" + attribute.getName() + '\'' +
                ", newValue=" + newValue +
                ", preventsCaching=" + preventsCaching +
                '}';
    }
}
