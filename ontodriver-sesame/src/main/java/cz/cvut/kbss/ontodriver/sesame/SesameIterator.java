package cz.cvut.kbss.ontodriver.sesame;

import org.openrdf.model.Resource;

import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;

interface SesameIterator {

	boolean hasNext() throws SesameDriverException;

	Resource nextNode() throws SesameDriverException;

	Resource nextContent() throws SesameDriverException;

	Axiom<java.net.URI> nextAxiom() throws SesameDriverException;

	void remove() throws SesameDriverException;

	void replaceCurrentWith(NamedResource newNode) throws SesameDriverException;
}
