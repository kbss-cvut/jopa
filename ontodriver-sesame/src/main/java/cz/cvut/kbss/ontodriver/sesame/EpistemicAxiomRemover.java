package cz.cvut.kbss.ontodriver.sesame;

import java.util.Collection;
import java.util.HashSet;

import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.ValueFactory;

import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver_new.AxiomDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Assertion;

/**
 * This class performs an epistemic remove of axioms described by the axiom
 * descriptor. </p>
 * 
 * Epistemic remove means that only information known to the application is
 * deleted. The assertions in the descriptor represent this information. Thus,
 * only these assertions are removed from the ontology. Note that if the
 * descriptor contains an unspecified property assertion, all property
 * assertions related to the subject individual are removed from the property's
 * context.
 * 
 * @author ledvima1
 * 
 */
class EpistemicAxiomRemover {

	private final Connector connector;
	private final ValueFactory valueFactory;

	EpistemicAxiomRemover(Connector connector, ValueFactory valueFactory) {
		this.connector = connector;
		this.valueFactory = valueFactory;
	}

	void remove(AxiomDescriptor axiomDescriptor) throws SesameDriverException {
		final Resource individual = SesameUtils.toSesameUri(axiomDescriptor.getSubject()
				.getIdentifier(), valueFactory);
		final Collection<Statement> toRemove = new HashSet<>();
		for (Assertion a : axiomDescriptor.getAssertions()) {
			if (a.isInferred()) {
				continue;
			}
			toRemove.addAll(connector.findStatements(individual,
					SesameUtils.toSesameUri(a.getIdentifier(), valueFactory), null, false,
					SesameUtils.toSesameUri(axiomDescriptor.getAssertionContext(a), valueFactory)));
		}
		connector.removeStatements(toRemove);
	}
}
