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
package cz.cvut.kbss.ontodriver.owlapi.connector;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.owlapi.OwlapiDataSource;
import cz.cvut.kbss.ontodriver.owlapi.change.MutableAddAxiom;
import cz.cvut.kbss.ontodriver.owlapi.config.OwlapiConfigParam;
import cz.cvut.kbss.ontodriver.owlapi.environment.Generator;
import cz.cvut.kbss.ontodriver.owlapi.exception.InvalidOntologyIriException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.AddAxiom;
import org.semanticweb.owlapi.model.AddImport;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassAssertionAxiom;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLImportsDeclaration;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;

import java.io.File;
import java.net.URI;
import java.nio.file.Files;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class BasicStorageConnectorTest {

    private static final URI ONTOLOGY_URI = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/connector");

    private BasicStorageConnector connector;

    private OWLOntologyManager manager;
    private OWLOntology ontology;

    @AfterEach
    public void tearDown() throws Exception {
        if (connector != null) {
            connector.close();
        }
    }

    private OntologyStorageProperties initStorageProperties(URI filePath, URI logicalUri) {
        return OntologyStorageProperties.ontologyUri(logicalUri).physicalUri(
                filePath).driver(OwlapiDataSource.class.getCanonicalName()).build();
    }

    @Test
    public void loadsExistingOntology() throws Exception {
        final URI physicalUri = initOntology(Collections.emptySet(), false);
        this.connector = new BasicStorageConnector(
                new DriverConfiguration(initStorageProperties(physicalUri, ONTOLOGY_URI)));
        assertNotNull(connector);
        assertTrue(connector.isOpen());
        final OntologySnapshot snapshot = connector.getOntologySnapshot();
        assertNotNull(snapshot.ontology());
        assertNotNull(snapshot.ontologyManager());
        assertNotNull(snapshot.dataFactory());
    }

    private URI initOntology(Set<OWLAxiom> axioms, boolean anonymous) throws Exception {
        final File targetFile = Files.createTempFile("connectortest", ".owl").toFile();
        targetFile.deleteOnExit();
        final OWLOntologyManager om = OWLManager.createOWLOntologyManager();
        final OWLOntology o = anonymous ? om.createOntology() : om.createOntology(IRI.create(ONTOLOGY_URI));
        om.addAxioms(o, axioms.stream());
        om.saveOntology(o, IRI.create(targetFile));
        this.manager = om;
        this.ontology = o;
        return targetFile.toURI();
    }

    @Test
    public void throwsExceptionWhenLoadedOntologyHasDifferentIri() throws Exception {
        final URI physicalUri = initOntology(Collections.emptySet(), false);
        final URI logicalUri = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/different");
        assertThrows(InvalidOntologyIriException.class, () -> new BasicStorageConnector(new DriverConfiguration(initStorageProperties(physicalUri, logicalUri))));
    }

    @Test
    public void createsNewFileForOntologyWithNonExistentPhysicalLocation() throws Exception {
        final File f = new File(System.getProperty(
                "java.io.tmpdir") + File.separator + "connectortest" + System.currentTimeMillis() + ".owl");
        assertFalse(f.exists());
        final URI physicalUri = f.toURI();
        this.connector = new BasicStorageConnector(
                new DriverConfiguration(initStorageProperties(physicalUri, ONTOLOGY_URI)));
        assertNotNull(connector);
        assertTrue(f.exists());
        f.deleteOnExit();
    }

    @Test
    public void getSnapshotReturnsDistinctSnapshots() throws Exception {
        final URI physicalUri = initOntology(Collections.emptySet(), false);
        this.connector = new BasicStorageConnector(
                new DriverConfiguration(initStorageProperties(physicalUri, ONTOLOGY_URI)));
        final OntologySnapshot snapshotOne = connector.getOntologySnapshot();
        final OntologySnapshot snapshotTwo = connector.getOntologySnapshot();

        assertNotSame(snapshotOne.ontology(), snapshotTwo.ontology());
    }

    @Test
    public void throwsExceptionWhenTryingToGetSnapshotOfClosedConnector() throws Exception {
        final URI physicalUri = initOntology(Collections.emptySet(), false);
        this.connector = new BasicStorageConnector(
                new DriverConfiguration(initStorageProperties(physicalUri, ONTOLOGY_URI)));
        connector.close();
        assertFalse(connector.isOpen());
        assertThrows(IllegalStateException.class, () -> connector.getOntologySnapshot());
    }

    @Test
    public void throwsExceptionWhenApplyChangesCalledOnClose() throws Exception {
        final URI physicalUri = initOntology(Collections.emptySet(), false);
        this.connector = new BasicStorageConnector(
                new DriverConfiguration(initStorageProperties(physicalUri, ONTOLOGY_URI)));
        connector.close();
        assertFalse(connector.isOpen());
        assertThrows(IllegalStateException.class, () -> connector.applyChanges(Collections.emptyList()));
    }

    @Test
    public void applyChangesModifiesTheCentralOntology() throws Exception {
        final URI physicalUri = initOntology(Collections.emptySet(), false);
        this.connector = new BasicStorageConnector(
                new DriverConfiguration(initStorageProperties(physicalUri, ONTOLOGY_URI)));
        final OntologySnapshot snapshot = connector.getOntologySnapshot();
        final OWLClass cls = addClassToOntology(snapshot);
        final OntologySnapshot result = connector.getOntologySnapshot();
        assertTrue(result.ontology().containsClassInSignature(cls.getIRI()));
    }

    private OWLClass addClassToOntology(OntologySnapshot snapshot) {
        final OWLClass cls = snapshot.dataFactory().getOWLClass(
                IRI.create("http://krizik.felk.cvut.cz/ontologies/jopa#OWClassA"));
        final OWLAxiom classDeclaration = snapshot.dataFactory().getOWLDeclarationAxiom(cls);
        final MutableAddAxiom add = new MutableAddAxiom(snapshot.ontology(), classDeclaration);

        connector.applyChanges(Collections.singletonList(add));
        return cls;
    }

    @Test
    public void successfullySavesOntologyOnClose() throws Exception {
        final URI physicalUri = initOntology(Collections.emptySet(), false);
        final OntologyStorageProperties storageProperties = initStorageProperties(physicalUri, ONTOLOGY_URI);
        this.connector = new BasicStorageConnector(new DriverConfiguration(storageProperties));
        final OWLClass cls = addClassToOntology(connector.getOntologySnapshot());
        connector.close();

        this.connector = new BasicStorageConnector(new DriverConfiguration(storageProperties));
        final OntologySnapshot res = connector.getOntologySnapshot();
        assertTrue(res.ontology().containsClassInSignature(cls.getIRI()));
    }

    @Test
    public void getSnapshotCreatesNewAnonymousTransactionalOntology() throws Exception {
        final URI physicalUri = initOntology(Collections.emptySet(), false);
        final OntologyStorageProperties storageProperties = initStorageProperties(physicalUri, ONTOLOGY_URI);
        this.connector = new BasicStorageConnector(new DriverConfiguration(storageProperties));
        final OntologySnapshot snapshot = connector.getOntologySnapshot();
        assertTrue(snapshot.ontology().getOntologyID().isAnonymous());
    }

    @Test
    public void getSnapshotCopiesAxiomsIntoTheTransactionOntology() throws Exception {
        final Set<OWLAxiom> axioms = Generator.generateAxioms();
        final URI physicalUri = initOntology(axioms, false);
        final OntologyStorageProperties storageProperties = initStorageProperties(physicalUri, ONTOLOGY_URI);
        this.connector = new BasicStorageConnector(new DriverConfiguration(storageProperties));
        final OntologySnapshot snapshot = connector.getOntologySnapshot();
        final Set<OWLAxiom> transactionalAxioms = snapshot.ontology().axioms().collect(Collectors.toSet());
        assertTrue(transactionalAxioms.containsAll(axioms));
    }

    @Test
    public void closeSnapshotRemovesTransactionalOntologyFromManager() throws Exception {
        final URI physicalUri = initOntology(Collections.emptySet(), false);
        final OntologyStorageProperties storageProperties = initStorageProperties(physicalUri, ONTOLOGY_URI);
        this.connector = new BasicStorageConnector(new DriverConfiguration(storageProperties));
        final OntologySnapshot snapshot = connector.getOntologySnapshot();
        final OWLOntology transactionalOntology = snapshot.ontology();
        final OWLOntologyManager manager = snapshot.ontologyManager(); // We know this is the root manager
        assertTrue(manager.contains(transactionalOntology));
        connector.closeSnapshot(snapshot);
        assertFalse(manager.contains(transactionalOntology));
    }

    @Test
    public void getSnapshotResolvesImportsOfTheTransactionalSnapshot() throws Exception {
        final URI physicalUri = initOntology(Collections.emptySet(), false);
        final String importedOntoLocation = "https://www.w3.org/TR/2003/PR-owl-guide-20031215/wine";
        final IRI importedOntoIri = IRI.create("http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine");
        final OWLImportsDeclaration importDecl = manager.getOWLDataFactory()
                                                        .getOWLImportsDeclaration(IRI.create(importedOntoLocation));
        manager.applyChange(new AddImport(ontology, importDecl));
        manager.saveOntology(ontology, IRI.create(physicalUri));
        final OntologyStorageProperties storageProperties = initStorageProperties(physicalUri, ONTOLOGY_URI);
        this.connector = new BasicStorageConnector(new DriverConfiguration(storageProperties));
        final OntologySnapshot snapshot = connector.getOntologySnapshot();
        final Stream<OWLOntology> imports = snapshot.ontology().imports();
        final Optional<OWLOntology> imported =
                imports.filter(imp -> imp.getOntologyID().getOntologyIRI().orElse(IRI.create(""))
                                         .equals(importedOntoIri)).findAny();
        assertTrue(imported.isPresent());
    }

    @Test
    public void reloadStorageReloadsOntologyFromFile() throws Exception {
        final URI physicalUri = initOntology(Collections.emptySet(), false);
        final OntologyStorageProperties storageProperties = initStorageProperties(physicalUri, ONTOLOGY_URI);
        this.connector = new BasicStorageConnector(new DriverConfiguration(storageProperties));
        final IRI clsIri = IRI.create(Generator.generateUri());
        final IRI individual = IRI.create(Generator.generateUri());
        connector.executeRead(snapshot -> {
            assertFalse(snapshot.ontology().containsClassInSignature(clsIri));
            return null;
        });
        final OWLDataFactory df = manager.getOWLDataFactory();
        final OWLClassAssertionAxiom clsAxiom =
                df.getOWLClassAssertionAxiom(df.getOWLClass(clsIri), df.getOWLNamedIndividual(individual));
        manager.applyChange(new AddAxiom(ontology, clsAxiom));
        manager.saveOntology(ontology, IRI.create(physicalUri));

        connector.reloadData();
        connector.executeRead(snapshot -> {
            assertTrue(snapshot.ontology().containsClassInSignature(clsIri));
            return null;
        });
    }

    @Test
    public void loadsExistingAnonymousOntology() throws Exception {
        final URI physicalUri = initOntology(Collections.emptySet(), true);
        this.connector = new BasicStorageConnector(new DriverConfiguration(initStorageProperties(physicalUri, null)));
        assertNotNull(connector);
        assertTrue(connector.isOpen());
        final OntologySnapshot snapshot = connector.getOntologySnapshot();
        assertNotNull(snapshot.ontology());
        assertTrue(snapshot.ontology().getOntologyID().isAnonymous());
        assertNotNull(snapshot.ontologyManager());
        assertNotNull(snapshot.dataFactory());
    }

    @Test
    void ontologyIsNotSavedWhenVolatileStorageIsConfigured() throws OntoDriverException {
        final File nonExistent = new File(System.getProperty("java.io.tmpdir") + File.separator + "non-existent.owl");
        assertFalse(nonExistent.exists());
        try {
            final OntologyStorageProperties storageProperties = initStorageProperties(nonExistent.toURI(), ONTOLOGY_URI);
            final DriverConfiguration driverConfig = new DriverConfiguration(storageProperties);
            driverConfig.setProperty(OwlapiConfigParam.USE_VOLATILE_STORAGE, "true");
            this.connector = new BasicStorageConnector(driverConfig);
            connector.close();
            assertFalse(nonExistent.exists());
        } finally {
            nonExistent.delete();
        }
    }

    @Test
    void writeToFileAllowsWritingCurrentOntologyStateToSpecifiedFile() throws Exception {
        final File nonExistent = new File(System.getProperty("java.io.tmpdir") + File.separator + "non-existent.owl");
        final File targetFile = new File(System.getProperty("java.io.tmpdir") + File.separator + "write-to-file-test.owl");
        assertFalse(targetFile.exists());
        try {
            final OntologyStorageProperties storageProperties = initStorageProperties(nonExistent.toURI(), ONTOLOGY_URI);
            final DriverConfiguration driverConfig = new DriverConfiguration(storageProperties);
            driverConfig.setProperty(OwlapiConfigParam.USE_VOLATILE_STORAGE, "true");
            this.connector = new BasicStorageConnector(driverConfig);
            final OWLDataFactory df = new OWLDataFactoryImpl();
            final OWLClass cls = df.getOWLClass(IRI.create("http://krizik.felk.cvut.cz/ontologies/jopa#OWClassA"));
            final OWLAxiom classDeclaration = df.getOWLDeclarationAxiom(cls);
            final OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
            final MutableAddAxiom add = new MutableAddAxiom(manager.createOntology(), classDeclaration);
            connector.applyChanges(List.of(add));
            connector.writeToFile(targetFile.getAbsolutePath());
            assertTrue(targetFile.exists());
            final OWLOntology o = manager.loadOntology(IRI.create(targetFile));
            assertEquals(o.axioms().count(), o.axioms().count());
            manager.clearOntologies();
            connector.close();
        } finally {
            nonExistent.delete();
            targetFile.delete();
        }
    }
}
