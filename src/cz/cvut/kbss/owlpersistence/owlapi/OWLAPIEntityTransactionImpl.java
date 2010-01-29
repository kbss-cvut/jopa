package cz.cvut.kbss.owlpersistence.owlapi;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChange;
import org.semanticweb.owlapi.model.OWLOntologyChangeException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;

import cz.cvut.kbss.owlpersistence.model.EntityTransaction;

public class OWLAPIEntityTransactionImpl implements EntityTransaction {

	private static final Logger LOG = Logger.getLogger(EntityTransaction.class
			.getName());

	private final OWLOntologyManager m;

	private final OWLOntology o;

	private List<OWLOntologyChange> changes = new ArrayList<OWLOntologyChange>();

	private boolean rollbackOnly;

	private boolean active;

	public OWLAPIEntityTransactionImpl(final OWLOntology o) {
		this.o = o;
		this.m = o.getOWLOntologyManager();
	}

	void addChange(final OWLOntologyChange c) {
		changes.add(c);
	}

	private void ensureActive() {
		if (isActive()) {
			throw new IllegalStateException();
		}
	}

	private void ensureInactive() {
		if (!isActive()) {
			throw new IllegalStateException();
		}
	}

	@Override
	public void begin() {
		ensureInactive();

		// TODO
	}

	@Override
	public synchronized void commit() {
		ensureActive();

		try {
			m.applyChanges(changes);
			for (final OWLOntology o : m.getOntologies()) {
				m.saveOntology(o);
			}
		} catch (OWLOntologyStorageException e) {
			LOG.log(Level.SEVERE, e.getMessage(), e);
		} catch (OWLOntologyChangeException e) {
			e.printStackTrace();
		}

		clear();
	}

	private void clear() {
		changes.clear();
		active = false;
	}

	@Override
	public synchronized boolean getRollbackOnly() {
		ensureActive();
		return rollbackOnly;
	}

	@Override
	public synchronized boolean isActive() {
		return active;
	}

	@Override
	public synchronized void rollback() {
		ensureActive();
		clear();
	}

	@Override
	public synchronized void setRollbackOnly() {
		ensureActive();
		this.rollbackOnly = true;
	}
}
