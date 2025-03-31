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
package cz.cvut.kbss.ontodriver;

import java.net.URI;
import java.util.Objects;
import java.util.Optional;

/**
 * Holds properties of an ontology storage.
 * <p>
 * These properties can be used to create a DataSource representing the storage.
 */
public class OntologyStorageProperties {

    /**
     * URI of the ontology.
     * <p>
     * Logical URI is not required, since for example for Sesame storages there is no logical
     * URI, there is just the physical URI of the repository and multiple contexts in it.
     * <p>
     * However, OWLAPI based storages require logical URI.
     */
    private final URI ontologyUri;
    /**
     * URI of the physical storage, e. g. OWLDB database, OWLIM storage, file
     */
    private final URI physicalUri;
    /**
     * Fully qualified OntoDriver data source class name
     */
    private final String driver;
    /**
     * User name for the storage, if necessary
     */
    private final String username;
    /**
     * Password for the storage, if necessary
     */
    private final String password;

    private OntologyStorageProperties(OntologyStoragePropertiesBuilder builder) {
        this.physicalUri = Objects.requireNonNull(builder.physicalUri, "Ontology physical URI is required!");
        this.driver = Objects.requireNonNull(builder.driverClass, "OntDriver data source class name is required!");
        this.ontologyUri = builder.ontologyUri;
        this.username = builder.username;
        this.password = builder.password;
    }

    public Optional<URI> getOntologyURI() {
        return Optional.ofNullable(ontologyUri);
    }

    public URI getPhysicalURI() {
        return physicalUri;
    }

    public String getDriver() {
        return driver;
    }

    public String getUsername() {
        return username;
    }

    public String getPassword() {
        return password;
    }

    @Override
    public String toString() {
        final StringBuilder b = new StringBuilder();
        b.append("StorageProperties: logical URI = ");
        b.append(ontologyUri);
        b.append(", physical URI = ");
        b.append(physicalUri);
        b.append(", data source class = ");
        b.append(driver);
        if (username != null) {
            b.append(", username = ");
            b.append(username);
            b.append(", password = ");
            b.append(password);
        }
        return b.toString();
    }

    public static OntologyStoragePropertiesBuilder ontologyUri(URI ontologyUri) {
        return new OntologyStoragePropertiesBuilder().ontologyUri(ontologyUri);
    }

    public static OntologyStoragePropertiesBuilder physicalUri(URI physicalUri) {
        return new OntologyStoragePropertiesBuilder().physicalUri(physicalUri);
    }

    public static OntologyStoragePropertiesBuilder username(String username) {
        return new OntologyStoragePropertiesBuilder().username(username);
    }

    public static OntologyStoragePropertiesBuilder password(String password) {
        return new OntologyStoragePropertiesBuilder().password(password);
    }

    public static OntologyStoragePropertiesBuilder driver(String driverClass) {
        return new OntologyStoragePropertiesBuilder().driver(driverClass);
    }

    /**
     * Builder class for the {@code OntologyStorageProperties}.
     */
    public static class OntologyStoragePropertiesBuilder {

        private URI ontologyUri;
        private URI physicalUri;
        private String username;
        private String password;
        private String driverClass;

        public OntologyStoragePropertiesBuilder ontologyUri(URI ontologyUri) {
            this.ontologyUri = ontologyUri;
            return this;
        }

        public OntologyStoragePropertiesBuilder ontologyUri(String ontologyUri) {
            this.ontologyUri = ontologyUri != null ? URI.create(ontologyUri) : null;
            return this;
        }

        public OntologyStoragePropertiesBuilder physicalUri(URI physicalUri) {
            this.physicalUri = physicalUri;
            return this;
        }

        public OntologyStoragePropertiesBuilder physicalUri(String physicalUri) {
            this.physicalUri = physicalUri != null ? URI.create(physicalUri) : null;
            return this;
        }

        public OntologyStoragePropertiesBuilder username(String username) {
            this.username = username;
            return this;
        }

        public OntologyStoragePropertiesBuilder password(String password) {
            this.password = password;
            return this;
        }

        public OntologyStoragePropertiesBuilder driver(String driverClass) {
            this.driverClass = driverClass;
            return this;
        }

        public OntologyStorageProperties build() {
            return new OntologyStorageProperties(this);
        }
    }
}
