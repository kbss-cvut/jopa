package cz.cvut.kbss.owlpersistence.owlapi;

import cz.cvut.kbss.owlpersistence.model.OWLPersistenceException;

@SuppressWarnings("serial")
public class NotYetImplementedException extends OWLPersistenceException {

	public NotYetImplementedException() {
		super("Not Yet Implemented.");
	}
}
