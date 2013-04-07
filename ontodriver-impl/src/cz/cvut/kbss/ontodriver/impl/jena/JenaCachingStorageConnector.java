package cz.cvut.kbss.ontodriver.impl.jena;

import java.util.Set;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;

import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.impl.owlapi.OwlapiConnectorDataHolder;

public class JenaCachingStorageConnector implements OwlapiBasedJenaConnector {

	private static final Logger LOG = Logger.getLogger(JenaCachingStorageConnector.class.getName());

	private static final ReentrantReadWriteLock LOCK = new ReentrantReadWriteLock();
	private static final Lock READ = LOCK.readLock();
	private static final Lock WRITE = LOCK.writeLock();

	private final OwlapiBasedJenaConnector connector;

	private boolean open;

	public JenaCachingStorageConnector(OwlapiBasedJenaConnector connector) {
		if (connector == null) {
			throw new NullPointerException();
		}
		this.connector = connector;
	}

	@Override
	public void close() throws OntoDriverException {
		if (!open) {
			return;
		}
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Closing Jena caching storage connector.");
		}
		this.open = false;
	}

	@Override
	public boolean isOpen() {
		return open;
	}

	@Override
	public void reload() throws OntoDriverException {
		// Do nothing
	}

	@Override
	public OwlapiConnectorDataHolder getOntologyDataInOwlapi() throws OntoDriverException {
		return cloneOntologyDataInOwlapi();
	}

	@Override
	public int getClassAssertionAxiomsCount() {
		READ.lock();
		try {
			return connector.getClassAssertionAxiomsCount();
		} finally {
			READ.unlock();
		}
	}

	@Override
	public OwlapiConnectorDataHolder cloneOntologyDataInOwlapi() throws OntoDriverException {
		READ.lock();
		try {
			final OwlapiConnectorDataHolder orig = connector.getOntologyDataInOwlapi();
			final Set<OWLAxiom> axioms = orig.getWorkingOntology().getAxioms();
			final OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
			final OWLDataFactory factory = manager.getOWLDataFactory();
			OWLOntology ontology = null;
			try {
				ontology = manager.createOntology(orig.getWorkingOntology().getOntologyID());
			} catch (OWLOntologyCreationException e) {
				throw new OntoDriverException("Unable to clone working ontology.", e);
			}
			manager.addAxioms(ontology, axioms);
			final OwlapiConnectorDataHolder holder = OwlapiConnectorDataHolder
					.workingOntology(ontology).reasoningOntology(orig.getReasoningOntology())
					.ontologyManager(manager).dataFactory(factory).reasoner(orig.getReasoner())
					.language(orig.getLanguage()).build();
			return holder;
		} finally {
			READ.unlock();
		}
	}

	@Override
	public void applyOntologyChanges(OWLOntologyManager manager, OWLOntology ontology)
			throws OntoDriverException {
		if (manager == null || ontology == null) {
			throw new NullPointerException();
		}
		WRITE.lock();
		try {
			connector.applyOntologyChanges(manager, ontology);
		} finally {
			WRITE.unlock();
		}
	}

	@Override
	public void saveOntology() throws OntoDriverException {
		WRITE.lock();
		try {
			connector.saveOntology();
		} finally {
			WRITE.unlock();
		}
	}

}
