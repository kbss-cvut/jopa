package cz.cvut.kbss.owlpersistence.owl2java;

import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

import cz.cvut.kbss.owlpersistence.model.ic.DataParticipationConstraint;
import cz.cvut.kbss.owlpersistence.model.ic.IntegrityConstraintVisitor;
import cz.cvut.kbss.owlpersistence.model.ic.ObjectParticipationConstraint;

public class ICSatisfiabilityChecker implements IntegrityConstraintVisitor {

	private OWLReasoner r;
	private OWLDataFactory f;

	private boolean result;

	public ICSatisfiabilityChecker(final OWLReasoner r, final OWLDataFactory f) {
		this.f = f;
		this.r = r;
	}

	@Override
	public void visit(DataParticipationConstraint cpc) {
		result = r.isSatisfiable(f.getOWLObjectIntersectionOf(cpc.getSubject(),
				f.getOWLDataMaxCardinality(cpc.getMax(), cpc.getPredicate(),
						cpc.getObject()), f.getOWLDataMinCardinality(cpc
						.getMin(), cpc.getPredicate())));
	}

	@Override
	public void visit(ObjectParticipationConstraint cpc) {
		result = r.isSatisfiable(f.getOWLObjectIntersectionOf(cpc.getSubject(),
				f.getOWLObjectMaxCardinality(cpc.getMax(), cpc.getPredicate(),
						cpc.getObject()), f.getOWLObjectMinCardinality(cpc
						.getMin(), cpc.getPredicate())));
	}

	public boolean getResult() {
		return result;
	}

}
