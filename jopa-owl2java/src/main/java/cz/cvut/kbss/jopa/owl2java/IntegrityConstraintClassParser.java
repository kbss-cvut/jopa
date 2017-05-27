package cz.cvut.kbss.jopa.owl2java;

import cz.cvut.kbss.jopa.ic.api.IntegrityConstraint;
import cz.cvut.kbss.jopa.ic.api.IntegrityConstraintFactory;
import cz.cvut.kbss.jopa.ic.impl.IntegrityConstraintFactoryImpl;
import java.util.HashSet;
import java.util.Set;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLClassExpressionVisitor;
import org.semanticweb.owlapi.model.OWLDataAllValuesFrom;
import org.semanticweb.owlapi.model.OWLDataExactCardinality;
import org.semanticweb.owlapi.model.OWLDataHasValue;
import org.semanticweb.owlapi.model.OWLDataMaxCardinality;
import org.semanticweb.owlapi.model.OWLDataMinCardinality;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLDataSomeValuesFrom;
import org.semanticweb.owlapi.model.OWLDatatype;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLObjectAllValuesFrom;
import org.semanticweb.owlapi.model.OWLObjectComplementOf;
import org.semanticweb.owlapi.model.OWLObjectExactCardinality;
import org.semanticweb.owlapi.model.OWLObjectHasSelf;
import org.semanticweb.owlapi.model.OWLObjectHasValue;
import org.semanticweb.owlapi.model.OWLObjectIntersectionOf;
import org.semanticweb.owlapi.model.OWLObjectMaxCardinality;
import org.semanticweb.owlapi.model.OWLObjectMinCardinality;
import org.semanticweb.owlapi.model.OWLObjectOneOf;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom;
import org.semanticweb.owlapi.model.OWLObjectUnionOf;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class IntegrityConstraintClassParser implements OWLClassExpressionVisitor {

    private static final Logger log = LoggerFactory.getLogger(IntegrityConstraintClassParser.class);

    private final Set<IntegrityConstraint> integrityConstraints = new HashSet<>();

    private OWLClass subjClass;

    private IntegrityConstraintFactory integrityConstraintFactory = new IntegrityConstraintFactoryImpl();

    public IntegrityConstraintClassParser(final OWLClass subjClass) {
        this.subjClass = subjClass;
    }

    private static void notSupported(final OWLObject o) {
        log.info("Ignoring Unsupported Axiom : {}", o);
    }

    public Set<IntegrityConstraint> getIntegrityConstraints() {
        return integrityConstraints;
    }

    public void visit(OWLDataMaxCardinality arg0) {
        try {
            final OWLDatatype dt = Utils.ensureDatatype(arg0.getFiller());
            final OWLDataProperty dp = Utils.ensureDataProperty(arg0
                .getProperty());

            integrityConstraints.add(integrityConstraintFactory
                .MaxDataParticipationConstraint(subjClass, dp, dt,
                    arg0.getCardinality()));
        } catch (UnsupportedICException e) {
            notSupported(arg0);
        }
    }

    public void visit(OWLDataExactCardinality arg0) {
        try {
            final OWLDatatype dt = Utils.ensureDatatype(arg0.getFiller());
            final OWLDataProperty dp = Utils.ensureDataProperty(arg0
                .getProperty());

            integrityConstraints.add(integrityConstraintFactory.DataParticipationConstraint(
                subjClass, dp, dt, arg0.getCardinality(),
                arg0.getCardinality()));
        } catch (UnsupportedICException e) {
            notSupported(arg0);
        }
    }

    public void visit(OWLDataMinCardinality arg0) {
        try {
            final OWLDatatype dt = Utils.ensureDatatype(arg0.getFiller());
            final OWLDataProperty dp = Utils.ensureDataProperty(arg0
                .getProperty());

            integrityConstraints.add(integrityConstraintFactory
                .MinDataParticipationConstraint(subjClass, dp, dt,
                    arg0.getCardinality()));
        } catch (UnsupportedICException e) {
            notSupported(arg0);
        }
    }

    public void visit(OWLDataHasValue arg0) {
        notSupported(arg0);
        //IntegrityConstraintParser.ensureDataProperty(arg0.getProperty());
        //
        // set.add(IntegrityConstraintFactoryImpl
        // .datatypeParticipationConstraint(subjClass, arg0
        // .getProperty().asOWLDataProperty(), f
        // .getOWLDataOneOf(arg0.getValue()), 1, 1));
    }

    public void visit(OWLDataAllValuesFrom arg0) {
        try {
            OWLDataProperty op = Utils.ensureDataProperty(arg0.getProperty());
            OWLDatatype clz = Utils.ensureDatatype(arg0.getFiller());

            integrityConstraints.add(integrityConstraintFactory.DataPropertyRangeConstraint(
                subjClass, op, clz));
        } catch (UnsupportedICException e) {
            notSupported(arg0);
        }
    }

    public void visit(OWLDataSomeValuesFrom arg0) {
        try {
            final OWLDatatype dt = Utils.ensureDatatype(arg0.getFiller());
            final OWLDataProperty dp = Utils.ensureDataProperty(arg0
                .getProperty());

            integrityConstraints.add(integrityConstraintFactory
                .MinDataParticipationConstraint(subjClass, dp, dt, 1));
        } catch (UnsupportedICException e) {
            notSupported(arg0);
        }
    }

    public void visit(OWLObjectOneOf arg0) {
        notSupported(arg0);
    }

    public void visit(OWLObjectHasSelf arg0) {
        notSupported(arg0);
    }

    public void visit(OWLObjectMaxCardinality arg0) {
        try {
            OWLClass c = Utils.ensureClass(arg0.getFiller());
            OWLObjectProperty p = Utils.ensureObjectProperty(arg0.getProperty());

            integrityConstraints.add(integrityConstraintFactory
                .MaxObjectParticipationConstraint(subjClass, p, c,
                    arg0.getCardinality()));
        } catch (UnsupportedICException e) {
            notSupported(arg0);
        }
    }

    public void visit(OWLObjectExactCardinality arg0) {
        try {
            OWLClass c = Utils.ensureClass(arg0.getFiller());
            OWLObjectProperty p = Utils.ensureObjectProperty(arg0.getProperty());

            integrityConstraints.add(integrityConstraintFactory
                .ObjectParticipationConstraint(subjClass, p, c,
                    arg0.getCardinality(), arg0.getCardinality()));
        } catch (UnsupportedICException e) {
            notSupported(arg0);
        }
    }

    public void visit(OWLObjectMinCardinality arg0) {
        try {
            OWLClass c = Utils.ensureClass(arg0.getFiller());
            OWLObjectProperty p = Utils.ensureObjectProperty(arg0.getProperty());

            integrityConstraints.add(integrityConstraintFactory
                .MinObjectParticipationConstraint(subjClass, p, c,
                    arg0.getCardinality()));
        } catch (UnsupportedICException e) {
            notSupported(arg0);
        }
    }

    public void visit(OWLObjectHasValue arg0) {
        notSupported(arg0);
    }

    public void visit(OWLObjectAllValuesFrom arg0) {
        try {
            OWLObjectProperty op = Utils.ensureObjectProperty(arg0.getProperty());
            OWLClass clz = Utils.ensureClass(arg0.getFiller());

            integrityConstraints.add(integrityConstraintFactory
                .ObjectPropertyRangeConstraint(subjClass, op, clz));
        } catch (UnsupportedICException e) {
            notSupported(arg0);
        }
    }

    public void visit(OWLObjectSomeValuesFrom arg0) {
        try {
            OWLClass c = Utils.ensureClass(arg0.getFiller());
            OWLObjectProperty p = Utils.ensureObjectProperty(arg0.getProperty());

            integrityConstraints.add(integrityConstraintFactory
                .MinObjectParticipationConstraint(subjClass, p, c, 1));
        } catch (UnsupportedICException e) {
            notSupported(arg0);
        }
    }

    public void visit(OWLObjectComplementOf arg0) {
        notSupported(arg0);
    }

    public void visit(OWLObjectUnionOf arg0) {
        notSupported(arg0);
    }

    public void visit(OWLObjectIntersectionOf arg0) {
        for (final OWLClassExpression e : arg0.getOperands()) {
            e.accept(this);
        }
    }

    public void visit(OWLClass arg0) {
        integrityConstraints.add(integrityConstraintFactory.SubClassConstraint(subjClass, arg0));
    }
}
