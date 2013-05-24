/**
 * Copyright (C) 2011 Czech Technical University in Prague
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

package cz.cvut.kbss.jopa.forms;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.UnknownOWLOntologyException;

import cz.cvut.kbss.jopa.Persistence;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.owlapi.EntityManagerFactoryImpl;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProvider;

public class TestEnvironment {
	private static final Logger log = Logger.getLogger(TestEnvironment.class
			.getName());

	public static final String dir = "/home/joudy/.NetBeansProjects/jopa/trunk/examples";
	public static final String DB_URI = "jdbc:postgresql://localhost/owldb";
	public static final String DB_USERNAME = "owldb";
	public static final String DB_PASSWORD = "owldb";

	private static final String REASONER_FACTORY_CLASS = "com.clarkparsia.pellet.owlapiv3.PelletReasonerFactory";

	/**
	 * True if the ontology file should be deleted before access to it is
	 * initialized. This effectively means that the test will create the
	 * ontology from scratch. Default value is true.
	 */
	public static boolean shouldDeleteOntologyFile = false;

	// private static final String REASONER_FACTORY_CLASS =
	// "org.semanticweb.HermiT.Reasoner$ReasonerFactory";

	public static enum Storage {
		OWLDB, FILE
	};

	public static EntityManager getPersistenceConnector(String name) {
		return getPersistenceConnector(name, Storage.FILE, true);
	}

	public static EntityManager getPersistenceConnector(String name,
			boolean cache) {
		return getPersistenceConnector(name, Storage.FILE, cache);
	}

	public static EntityManager getPersistenceConnector(String name,
			Storage storage, boolean cache) {
		final Map<String, String> params = new HashMap<String, String>();
		final IRI iri = IRI
				.create("http://krizik.felk.cvut.cz/ontologies/2010/"
						+ name);
		params.put(OWLAPIPersistenceProperties.ONTOLOGY_URI_KEY, iri.toString());
		try {
			switch (storage) {
			case FILE:
				// Ontology stored in a file
				final File url = new File(dir + "/" + name + ".owl");
				final String physicalURI = url.getPath();
				if (url.exists() && shouldDeleteOntologyFile) {
					url.delete();
				}
				params.put(
						OWLAPIPersistenceProperties.ONTOLOGY_PHYSICAL_URI_KEY,
						physicalURI);
				break;
			case OWLDB:
				// OWLDB ontology access
				params.put(
						OWLAPIPersistenceProperties.ONTOLOGY_PHYSICAL_URI_KEY,
						DB_URI);
			}

			params.put("javax.persistence.provider",
					EntityManagerFactoryImpl.class.getName());
			if (cache) {
				params.put("cache", "on");
			} else {
				params.put("cache", "off");
			}
			/* Set location of the entities (package) */
			params.put("location", "cz.cvut.kbss.jopa.owlapi");
			params.put(OWLAPIPersistenceProperties.JPA_PERSISTENCE_PROVIDER,
					OWLAPIPersistenceProvider.class.getName());
			// params.put(OWLAPIPersistenceProperties.ONTOLOGY_FILE_KEY, url
			// .getAbsolutePath());
			params.put(OWLAPIPersistenceProperties.REASONER_FACTORY_CLASS,
					REASONER_FACTORY_CLASS);

			return Persistence.createEntityManagerFactory("context-name",
					params).createEntityManager();
		} catch (UnknownOWLOntologyException e) {
			log.log(Level.SEVERE, e.getMessage(), e);
		}
		return null;
	}

	public static EntityManager getPersistenceConnectorWithMappingFile(
			final String name, final Storage storage, final boolean cache) {
		final Map<String, String> params = new HashMap<String, String>();
		final IRI iri = IRI
				.create("http://krizik.felk.cvut.cz/ontologies/2010/"
						+ name);
		final String mappingFile = createMappingFile(name, iri, storage);
		params.put(OWLAPIPersistenceProperties.ONTOLOGY_URI_KEY, iri.toString());
		params.put(OWLAPIPersistenceProperties.MAPPING_FILE_URI_KEY,
				mappingFile);
		if (cache) {
			params.put("cache", "on");
		} else {
			params.put("cache", "off");
		}
		/* Set location of the entities (package) */
		params.put("location", "cz.cvut.kbss.jopa.owlapi");
		params.put(OWLAPIPersistenceProperties.JPA_PERSISTENCE_PROVIDER,
				OWLAPIPersistenceProvider.class.getName());
		// params.put(OWLAPIPersistenceProperties.ONTOLOGY_FILE_KEY, url
		// .getAbsolutePath());
		params.put(OWLAPIPersistenceProperties.REASONER_FACTORY_CLASS,
				REASONER_FACTORY_CLASS);

		return Persistence.createEntityManagerFactory("context-name", params)
				.createEntityManager();
	}

	private static String createMappingFile(final String name, final IRI iri,
			final Storage storage) {
		final File mappingFile = new File(dir + "/" + name + ".mapping");
		if (mappingFile.exists()) {
			mappingFile.delete();
		}
		final StringBuilder mapping = new StringBuilder();
		mapping.append(iri.toString());
		mapping.append(" > ");
		switch (storage) {
		case FILE:
			final File onto = new File(name + ".owl");
			if (shouldDeleteOntologyFile) {
				// We have to use the dir here, since the context path is
				// different
				final File f = new File(dir + "/" + name + ".owl");
				f.delete();
			}
			mapping.append(onto.getPath());
			break;
		case OWLDB:
			mapping.append(DB_URI);
			break;
		}

		try {
			mappingFile.createNewFile();

			final BufferedWriter wr = new BufferedWriter(new FileWriter(
					mappingFile));
			wr.write(mapping.toString());
			wr.close();
		} catch (IOException e) {
			log.log(Level.SEVERE, e.getMessage(), e);
		}
		return "file://" + mappingFile.getAbsolutePath();
	}

	public static Logger getLogger() {
		return log;
	}
}
