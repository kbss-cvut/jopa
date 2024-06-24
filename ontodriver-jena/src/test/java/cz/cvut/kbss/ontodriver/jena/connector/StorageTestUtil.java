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
package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.jena.JenaDataSource;
import org.apache.jena.query.Dataset;
import org.apache.jena.rdf.model.*;
import org.apache.jena.vocabulary.RDF;
import org.apache.jena.vocabulary.RDFS;

import java.io.File;

import static org.apache.jena.rdf.model.ResourceFactory.createProperty;
import static org.apache.jena.rdf.model.ResourceFactory.createResource;

class StorageTestUtil {

    static final String SUBJECT = "http://onto.fel.cvut.cz/ontologies/jena-driver/Resource";
    static final String TYPE_ONE = "http://onto.fel.cvut.cz/ontologies/jena-driver/TypeOne";
    static final String TYPE_TWO = "http://onto.fel.cvut.cz/ontologies/jena-driver/TypeTwo";
    static final String NAMED_GRAPH = "http://onto.fel.cvut.cz/ontologies/jena-driver/GraphOne";

    static final Resource RESOURCE = ResourceFactory.createResource(SUBJECT);

    static DriverConfiguration createConfiguration(String physicalUri) {
        final File f = new File(physicalUri);
        return new DriverConfiguration(
                OntologyStorageProperties.driver(JenaDataSource.class.toString()).physicalUri(f.toURI()).build());
    }

    static Statement statement(String subject, String property, String value) {
        return ResourceFactory
                .createStatement(createResource(subject), createProperty(property), createResource(value));
    }

    static void generateTestData(Dataset dataset) {
        final Model m = dataset.getDefaultModel();
        m.add(createResource(TYPE_ONE), RDFS.subClassOf, createResource(TYPE_TWO));
        m.add(RESOURCE, RDF.type, createResource(TYPE_ONE));
        final Model namedGraph = ModelFactory.createDefaultModel();
        namedGraph.add(RESOURCE, RDF.type, createResource(TYPE_TWO));
        dataset.addNamedModel(NAMED_GRAPH, namedGraph);
    }

    static void deleteStorageDir(File directory) {
        if (directory.exists()) {
            if (directory.listFiles() != null) {
                for (File f : directory.listFiles()) {
                    f.delete();
                }
            }
            directory.delete();
        }
    }
}
