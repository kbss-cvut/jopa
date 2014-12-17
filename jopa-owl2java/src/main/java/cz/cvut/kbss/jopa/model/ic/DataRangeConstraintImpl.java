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

package cz.cvut.kbss.jopa.model.ic;

import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLDatatype;

class DataRangeConstraintImpl implements
		DataRangeConstraint {

	private final OWLClass subj;
	private final OWLDataProperty p;
	private final OWLDatatype range;

	public DataRangeConstraintImpl(OWLClass subj, OWLDataProperty p, OWLDatatype range) {
		this.subj = subj;
		this.p = p;
		this.range = range;
	}

	public OWLDataProperty getProperty() {
		return p;
	}
	
	
	public OWLDatatype getRange() {
		return range;
	}

	
	public void accept(IntegrityConstraintVisitor visitor) {
		visitor.visit(this);
	}

	
	public OWLClass getOWLClass() {
		return subj;
	}
}
