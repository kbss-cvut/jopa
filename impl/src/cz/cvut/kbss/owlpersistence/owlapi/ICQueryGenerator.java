package cz.cvut.kbss.owlpersistence.owlapi;

import java.util.ArrayList;
import java.util.List;

import org.semanticweb.owlapi.model.OWLObject;

import cz.cvut.kbss.owl2query.model.OWL2Ontology;
import cz.cvut.kbss.owl2query.model.OWL2Query;
import cz.cvut.kbss.owl2query.model.OWL2QueryFactory;
import cz.cvut.kbss.owl2query.model.Term;
import cz.cvut.kbss.owl2query.model.Variable;
import cz.cvut.kbss.owlpersistence.model.ic.DataDomainConstraint;
import cz.cvut.kbss.owlpersistence.model.ic.DataParticipationConstraint;
import cz.cvut.kbss.owlpersistence.model.ic.DataRangeConstraint;
import cz.cvut.kbss.owlpersistence.model.ic.IntegrityConstraintVisitor;
import cz.cvut.kbss.owlpersistence.model.ic.ObjectDomainConstraint;
import cz.cvut.kbss.owlpersistence.model.ic.ObjectParticipationConstraint;
import cz.cvut.kbss.owlpersistence.model.ic.ObjectRangeConstraint;

class ICQueryGenerator implements IntegrityConstraintVisitor {

	final OWL2QueryFactory<OWLObject> f;
	final OWL2Ontology<OWLObject> ont;
	OWL2Query<OWLObject> query;

	public ICQueryGenerator(
			final OWL2QueryFactory<OWLObject> f,
			final OWL2Ontology<OWLObject> ont) {
		this.f = f;
		this.ont = ont;
		this.query = f.createQuery(ont);
	}

	@Override
	public void visit(DataRangeConstraint cpc) {
		Variable<OWLObject> x = f.variable("x");
		Variable<OWLObject> y = f.variable("y");

		final OWL2Query<OWLObject> not = f.createQuery(ont);
		not.addDistVar(y);

		query = query.Type(f.wrap(cpc.getOWLClass()), x).PropertyValue(
				f.wrap(cpc.getProperty()), x, y).Not(
				not.Type(f.wrap(cpc.getRange()), y));
		query.addDistVar(x);
		query.addDistVar(y);
	}

	@Override
	public void visit(DataDomainConstraint cpc) {
		Variable<OWLObject> x = f.variable("x");
		Variable<OWLObject> y = f.variable("y");

		final OWL2Query<OWLObject> not = f.createQuery(ont);
		not.addDistVar(y);

		query = query.Not(not.Type(f.wrap(cpc.getDomain()), x))
				.PropertyValue(f.wrap(cpc.getProperty()), x, y);
		query.addDistVar(x);
		query.addDistVar(y);
	}

	@Override
	public void visit(ObjectRangeConstraint cpc) {
		Variable<OWLObject> x = f.variable("x");
		Variable<OWLObject> y = f.variable("y");

		final OWL2Query<OWLObject> not = f.createQuery(ont);
		not.addDistVar(y);

		query = query.Type(f.wrap(cpc.getOWLClass()), x).PropertyValue(
				f.wrap(cpc.getProperty()), x, y).Not(
				not.Type(f.wrap(cpc.getRange()), y));
		query.addDistVar(x);
		query.addDistVar(y);
	}

	@Override
	public void visit(ObjectDomainConstraint cpc) {
		Variable<OWLObject> x = f.variable("x");
		Variable<OWLObject> y = f.variable("y");

		final OWL2Query<OWLObject> not = f.createQuery(ont);
		not.addDistVar(y);

		query = query.Not(not.Type(f.wrap(cpc.getDomain()), x))
				.PropertyValue(f.wrap(cpc.getProperty()), x, y);
		query.addDistVar(x);
		query.addDistVar(y);
	}

	@Override
	public void visit(ObjectParticipationConstraint cpc) {
		Variable<OWLObject> x = f.variable("x");
		query.addDistVar(x);

		query = query.Type(f.wrap(cpc.getSubject()), x);

		if (cpc.getMin() > 0) {
			query = min("y", cpc.getMin(), query, x, f.wrap(cpc
					.getPredicate()), f.wrap(cpc.getObject()));
		}

		if (cpc.getMax() > -1) {
			OWL2Query<OWLObject> notMax = f.createQuery(ont);
			notMax = min("z", cpc.getMax(), notMax, x, f.wrap(cpc
					.getPredicate()), f.wrap(cpc.getObject()));
			query = query.Not(notMax);
		}
	}

	private OWL2Query<OWLObject> min(String varPrefix, int n,
			OWL2Query<OWLObject> query, final Variable<OWLObject> varx,
			final Term<OWLObject> pred, final Term<OWLObject> obj) {
		final List<Variable<OWLObject>> vars = new ArrayList<Variable<OWLObject>>();
		for (int i = 1; i <= n; i++) {
			vars.add(f.variable(varPrefix + i));
		}

		for (int i = 1; i <= n; i++) {
			Variable<OWLObject> zi = vars.get(i - 1);
			query = query.PropertyValue(pred, varx, zi);
			query = query.Type(obj, zi);
			query.addDistVar(zi);

			for (int j = i; i <= n; i++) {
				final Variable<OWLObject> yj = vars.get(j - 1);
				final OWL2Query<OWLObject> not = f.createQuery(ont).SameAs(
						zi, yj);
				not.addDistVar(zi);
				not.addDistVar(yj);

				query = query.Not(not);
			}
		}
		return query;
	}

	@Override
	public void visit(DataParticipationConstraint cpc) {
		Variable<OWLObject> x = f.variable("x");
		query.addDistVar(x);

		query = query.Type(f.wrap(cpc.getSubject()), x);

		if (cpc.getMin() > 0) {
			query = min("y", cpc.getMin(), query, x, f.wrap(cpc
					.getPredicate()), f.wrap(cpc.getObject()));
		}

		if (cpc.getMax() > -1) {
			OWL2Query<OWLObject> notMax = f.createQuery(ont);
			notMax = min("z", cpc.getMax(), notMax, x, f.wrap(cpc
					.getPredicate()), f.wrap(cpc.getObject()));
			query = query.Not(notMax);
		}
	}

	public OWL2Query<OWLObject> getQuery() {
		return query;
	}
}