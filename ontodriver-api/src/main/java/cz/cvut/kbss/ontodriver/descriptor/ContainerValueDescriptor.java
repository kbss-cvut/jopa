/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.descriptor;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * Descriptor for saving values in an RDF container.
 *
 * @param <T> Value type
 */
public class ContainerValueDescriptor<T> extends ContainerDescriptor {

    private final List<T> values = new ArrayList<>();

    private ContainerValueDescriptor(Type type, NamedResource owner, Assertion property, URI context) {
        super(type, owner, property, context);
    }

    public List<T> getValues() {
        return Collections.unmodifiableList(values);
    }

    public void addValue(T value) {
        values.add(Objects.requireNonNull(value));
    }

    /**
     * Creates a new {@link ContainerValueDescriptor} for saving an {@literal rdf:Alt} container to the default
     * context.
     *
     * @param owner    Container owner
     * @param property Property referencing the container
     * @param <T>      Value type
     * @return Empty container value descriptor
     */
    public static <T> ContainerValueDescriptor<T> altValueDescriptor(NamedResource owner, Assertion property) {
        return new ContainerValueDescriptor<>(Type.ALT, owner, property, null);
    }

    /**
     * Creates a new {@link ContainerValueDescriptor} for saving an {@literal rdf:Alt} container to the specified
     * context.
     *
     * @param owner    Container owner
     * @param property Property referencing the container
     * @param context  Context to which the container should be saved
     * @param <T>      Value type
     * @return Empty container value descriptor
     */
    public static <T> ContainerValueDescriptor<T> altValueDescriptor(NamedResource owner, Assertion property,
                                                                     URI context) {
        return new ContainerValueDescriptor<>(Type.ALT, owner, property, context);
    }

    /**
     * Creates a new {@link ContainerValueDescriptor} for saving an {@literal rdf:Bag} container to the default
     * context.
     *
     * @param owner    Container owner
     * @param property Property referencing the container
     * @param <T>      Value type
     * @return Empty container value descriptor
     */
    public static <T> ContainerValueDescriptor<T> bagValueDescriptor(NamedResource owner, Assertion property) {
        return new ContainerValueDescriptor<>(Type.BAG, owner, property, null);
    }

    /**
     * Creates a new {@link ContainerValueDescriptor} for saving an {@literal rdf:Bag} container to the specified
     * context.
     *
     * @param owner    Container owner
     * @param property Property referencing the container
     * @param context  Context to which the container should be saved
     * @param <T>      Value type
     * @return Empty container value descriptor
     */
    public static <T> ContainerValueDescriptor<T> bagValueDescriptor(NamedResource owner, Assertion property,
                                                                     URI context) {
        return new ContainerValueDescriptor<>(Type.BAG, owner, property, context);
    }

    /**
     * Creates a new {@link ContainerValueDescriptor} for saving an {@literal rdf:Seq} container to the default
     * context.
     *
     * @param owner    Container owner
     * @param property Property referencing the container
     * @param <T>      Value type
     * @return Empty container value descriptor
     */
    public static <T> ContainerValueDescriptor<T> seqValueDescriptor(NamedResource owner, Assertion property) {
        return new ContainerValueDescriptor<>(Type.SEQ, owner, property, null);
    }

    /**
     * Creates a new {@link ContainerValueDescriptor} for saving an {@literal rdf:Seq} container to the specified
     * context.
     *
     * @param owner    Container owner
     * @param property Property referencing the container
     * @param context  Context to which the container should be saved
     * @param <T>      Value type
     * @return Empty container value descriptor
     */
    public static <T> ContainerValueDescriptor<T> seqValueDescriptor(NamedResource owner, Assertion property,
                                                                     URI context) {
        return new ContainerValueDescriptor<>(Type.SEQ, owner, property, context);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof ContainerValueDescriptor<?> that)) {
            return false;
        }
        if (!super.equals(o)) {
            return false;
        }
        return Objects.equals(getValues(), that.getValues());
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), getValues());
    }
}
