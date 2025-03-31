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
import java.util.Objects;

/**
 * Descriptor for reading an RDF container.
 */
public class ContainerDescriptor {

    private final Type type;

    private final NamedResource owner;

    private final Assertion property;

    private final URI context;

    protected ContainerDescriptor(Type type, NamedResource owner, Assertion property, URI context) {
        this.type = type;
        this.owner = owner;
        this.property = property;
        this.context = context;
    }

    public URI getType() {
        return type.uri;
    }

    public NamedResource getOwner() {
        return owner;
    }

    public Assertion getProperty() {
        return property;
    }

    public URI getContext() {
        return context;
    }

    /**
     * Creates a new {@link ContainerDescriptor} for reading an {@literal rdf:Alt} container from the default context.
     *
     * @param owner    Container owner
     * @param property Property referencing the container
     * @return Container descriptor
     */
    public static ContainerDescriptor altDescriptor(NamedResource owner, Assertion property) {
        return new ContainerDescriptor(Type.ALT, owner, property, null);
    }

    /**
     * Creates a new {@link ContainerDescriptor} for reading an {@literal rdf:Alt} container from the specified
     * context.
     *
     * @param owner    Container owner
     * @param property Property referencing the container
     * @param context  Context from which the container should be read
     * @return Container descriptor
     */
    public static ContainerDescriptor altDescriptor(NamedResource owner, Assertion property, URI context) {
        return new ContainerDescriptor(Type.ALT, owner, property, context);
    }

    /**
     * Creates a new {@link ContainerDescriptor} for reading an {@literal rdf:Bag} container from the default context.
     *
     * @param owner    Container owner
     * @param property Property referencing the container
     * @return Container descriptor
     */
    public static ContainerDescriptor bagDescriptor(NamedResource owner, Assertion property) {
        return new ContainerDescriptor(Type.BAG, owner, property, null);
    }

    /**
     * Creates a new {@link ContainerDescriptor} for reading an {@literal rdf:Bag} container from the specified
     * context.
     *
     * @param owner    Container owner
     * @param property Property referencing the container
     * @param context  Context from which the container should be read
     * @return Container descriptor
     */
    public static ContainerDescriptor bagDescriptor(NamedResource owner, Assertion property, URI context) {
        return new ContainerDescriptor(Type.BAG, owner, property, context);
    }

    /**
     * Creates a new {@link ContainerDescriptor} for reading an {@literal rdf:Seq} container from the default context.
     *
     * @param owner    Container owner
     * @param property Property referencing the container
     * @return Container descriptor
     */
    public static ContainerDescriptor seqDescriptor(NamedResource owner, Assertion property) {
        return new ContainerDescriptor(Type.SEQ, owner, property, null);
    }

    /**
     * Creates a new {@link ContainerDescriptor} for reading an {@literal rdf:Seq} container from the specified
     * context.
     *
     * @param owner    Container owner
     * @param property Property referencing the container
     * @param context  Context from which the container should be read
     * @return Container descriptor
     */
    public static ContainerDescriptor seqDescriptor(NamedResource owner, Assertion property, URI context) {
        return new ContainerDescriptor(Type.SEQ, owner, property, context);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof ContainerDescriptor that)) {
            return false;
        }
        return type == that.type && Objects.equals(getOwner(),
                that.getOwner()) && Objects.equals(
                getProperty(), that.getProperty()) && Objects.equals(getContext(), that.getContext());
    }

    @Override
    public int hashCode() {
        return Objects.hash(type, getOwner(), getProperty(), getContext());
    }

    protected enum Type {

        ALT(URI.create("http://www.w3.org/1999/02/22-rdf-syntax-ns#Alt")),
        BAG(URI.create("http://www.w3.org/1999/02/22-rdf-syntax-ns#Bag")),
        SEQ(URI.create("http://www.w3.org/1999/02/22-rdf-syntax-ns#Seq"));

        private final URI uri;

        Type(URI uri) {
            this.uri = uri;
        }
    }
}
