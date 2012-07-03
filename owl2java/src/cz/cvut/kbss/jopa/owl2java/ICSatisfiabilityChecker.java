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

package cz.cvut.kbss.jopa.owl2java;

import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

import cz.cvut.kbss.jopa.model.ic.DataDomainConstraint;
import cz.cvut.kbss.jopa.model.ic.DataParticipationConstraint;
import cz.cvut.kbss.jopa.model.ic.DataRangeConstraint;
import cz.cvut.kbss.jopa.model.ic.IntegrityConstraintVisitor;
import cz.cvut.kbss.jopa.model.ic.ObjectDomainConstraint;
import cz.cvut.kbss.jopa.model.ic.ObjectParticipationConstraint;
import cz.cvut.kbss.jopa.model.ic.ObjectRangeConstraint;

public class ICSatisfiabilityChecker implements IntegrityConstraintVisitor {

	private OWLReasoner r;
	private OWLDataFactory f;

	private boolean result;

	public ICSatisfiabilityChecker(final OWLReasoner r, final OWLDataFactory f) {
		this.f = f;
		this.r = r;
	}

	public void visit(DataParticipationConstraint cpc) {
		result = r.isSatisfiable(f.getOWLObjectIntersectionOf(cpc.getSubject(),
				f.getOWLDataMaxCardinality(cpc.getMax(), cpc.getPredicate(),
						cpc.getObject()), f.getOWLDataMinCardinality(cpc
						.getMin(), cpc.getPredicate())));
	}

	public void visit(ObjectParticipationConstraint cpc) {
		result = r.isSatisfiable(f.getOWLObjectIntersectionOf(cpc.getSubject(),
				f.getOWLObjectMaxCardinality(cpc.getMax(), cpc.getPredicate(),
						cpc.getObject()), f.getOWLObjectMinCardinality(cpc
						.getMin(), cpc.getPredicate())));
	}

	public boolean getResult() {
		return result;
	}

	public void visit(ObjectDomainConstraint cpc) {
		// TODO Auto-generated method stub
		
	}

	public void visit(ObjectRangeConstraint cpc) {
		// TODO Auto-generated method stub
		
	}

	public void visit(DataDomainConstraint cpc) {
		// TODO Auto-generated method stub
		
	}

	public void visit(DataRangeConstraint cpc) {
		// TODO Auto-generated method stub
		
	}
}
