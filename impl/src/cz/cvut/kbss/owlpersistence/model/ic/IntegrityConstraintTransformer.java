package cz.cvut.kbss.owlpersistence.model.ic;

import org.semanticweb.owlapi.model.OWLObject;

import cz.cvut.kbss.owl2query.simpleversion.model.OWL2Query;

interface IntegrityConstraintTransformer {

	OWL2Query<OWLObject> getQuery(final IntegrityConstraint ic);

}
