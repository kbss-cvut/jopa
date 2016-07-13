/**
 * Copyright (C) 2016 Czech Technical University in Prague
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

import java.lang.annotation.Annotation;

import com.sun.codemodel.JAnnotationUse;

import cz.cvut.kbss.jopa.model.annotations.DomainOf;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.jopa.model.annotations.RangeOf;

public class AnnotationBuilder implements IntegrityConstraintVisitor {

	private JAnnotationUse a;

	private Annotation annotation = null;
	
	AnnotationBuilder(final JAnnotationUse a) {
		this.a = a;
	}
	
	public Annotation getAnnotation() {
		return annotation;
	}

	
	public void visit(DataParticipationConstraint cpc) {
		a.annotate(ParticipationConstraint.class).param("owlObjectIRI",
						cpc.getObject().getIRI().toString()).param(
						"min", cpc.getMin()).param("max", cpc.getMax());
	}

	
	public void visit(ObjectParticipationConstraint cpc) {
		a.annotate(ParticipationConstraint.class).param("owlObjectIRI",
				cpc.getObject().getIRI().toString()).param(
				"min", cpc.getMin()).param("max", cpc.getMax());	}

	
	public void visit(ObjectDomainConstraint cpc) {
		a.annotate(DomainOf.class).param("owlPropertyIRI()",
				cpc.getProperty().getIRI().toString());
	}

	
	public void visit(ObjectRangeConstraint cpc) {
		a.annotate(RangeOf.class).param("owlPropertyIRI()",
				cpc.getProperty().getIRI().toString());
	}

	
	public void visit(DataDomainConstraint cpc) {
		a.annotate(DomainOf.class).param("owlPropertyIRI()",
				cpc.getProperty().getIRI().toString());
	}

	
	public void visit(DataRangeConstraint cpc) {
	}
}
