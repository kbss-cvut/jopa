package cz.cvut.kbss.ontodriver.sesame;

import org.openrdf.model.Resource;

import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver_new.model.Axiom;

interface SesameIterator {

	boolean hasNext();

	Resource next() throws SesameDriverException;

	Axiom<java.net.URI> nextAxiom() throws SesameDriverException;
}
