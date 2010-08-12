package cz.cvut.kbss.owlpersistence.model.ic;

import java.lang.annotation.Annotation;

import com.sun.codemodel.JAnnotationUse;

import cz.cvut.kbss.owlpersistence.model.annotations.DomainOf;
import cz.cvut.kbss.owlpersistence.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.owlpersistence.model.annotations.RangeOf;

public class AnnotationBuilder implements IntegrityConstraintVisitor {

	private JAnnotationUse a;
	
	AnnotationBuilder(final JAnnotationUse a) {
		this.a = a;
	}
	
	private Annotation annotation = null;
	
	public Annotation getAnnotation() {
		return annotation;
	}

	@Override
	public void visit(DataParticipationConstraint cpc) {
		a.annotate(ParticipationConstraint.class).param("owlObjectIRI",
						cpc.getObject().getIRI().toString()).param(
						"min", cpc.getMin()).param("max", cpc.getMax());
	}

	@Override
	public void visit(ObjectParticipationConstraint cpc) {
		a.annotate(ParticipationConstraint.class).param("owlObjectIRI",
				cpc.getObject().getIRI().toString()).param(
				"min", cpc.getMin()).param("max", cpc.getMax());	}

	@Override
	public void visit(ObjectDomainConstraint cpc) {
		a.annotate(DomainOf.class).param("owlPropertyIRI()",
				cpc.getProperty().getIRI().toString());
	}

	@Override
	public void visit(ObjectRangeConstraint cpc) {
		a.annotate(RangeOf.class).param("owlPropertyIRI()",
				cpc.getProperty().getIRI().toString());
	}

	@Override
	public void visit(DataDomainConstraint cpc) {
		a.annotate(DomainOf.class).param("owlPropertyIRI()",
				cpc.getProperty().getIRI().toString());
	}

	@Override
	public void visit(DataRangeConstraint cpc) {
	}
}
