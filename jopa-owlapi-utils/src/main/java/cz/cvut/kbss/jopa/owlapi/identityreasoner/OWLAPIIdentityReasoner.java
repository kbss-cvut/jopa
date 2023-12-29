/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.owlapi.identityreasoner;

import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.*;
import org.semanticweb.owlapi.reasoner.impl.*;
import org.semanticweb.owlapi.search.EntitySearcher;
import org.semanticweb.owlapi.util.Version;

import javax.annotation.Nonnull;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Deprecated
class OWLAPIIdentityReasoner implements OWLReasoner {

    private final OWLOntology o;

    public OWLAPIIdentityReasoner(final OWLOntology o) {
        this.o = o;
    }


    public void dispose() {
        // do nothing
    }


    public void flush() {
        // do nothing
    }


    @Nonnull
    public Node<OWLClass> getBottomClassNode() {
        return new OWLClassNode(o.getOWLOntologyManager().getOWLDataFactory().getOWLNothing());
    }


    @Nonnull
    public Node<OWLDataProperty> getBottomDataPropertyNode() {
        return new OWLDataPropertyNode(o.getOWLOntologyManager().getOWLDataFactory().getOWLBottomDataProperty());
    }


    @Nonnull
    public Node<OWLObjectPropertyExpression> getBottomObjectPropertyNode() {
        return new OWLObjectPropertyNode(o.getOWLOntologyManager().getOWLDataFactory().getOWLBottomObjectProperty());
    }


    @Nonnull
    public BufferingMode getBufferingMode() {
        return BufferingMode.NON_BUFFERING;
    }


    @Nonnull
    public NodeSet<OWLClass> getDataPropertyDomains(@Nonnull OWLDataProperty pe,
                                                    boolean direct) {
        final OWLClassNodeSet s = new OWLClassNodeSet();

        for (final OWLDataPropertyDomainAxiom a : o.getAxioms(AxiomType.DATA_PROPERTY_DOMAIN)) {
            if (a.getProperty().equals(pe) && !a.getDomain().isAnonymous()) {
                s.addEntity(a.getDomain().asOWLClass());
            }
        }

        return s;
    }


    @Nonnull
    public Set<OWLLiteral> getDataPropertyValues(@Nonnull OWLNamedIndividual ind,
                                                 @Nonnull OWLDataProperty pe) {
        final Set<OWLLiteral> literals = new HashSet<>();

        for (final OWLDataPropertyAssertionAxiom a : o.getAxioms(AxiomType.DATA_PROPERTY_ASSERTION)) {
            if (a.getProperty().equals(pe) && a.getSubject().equals(ind)) {
                literals.add(a.getObject());
            }
        }

        return literals;
    }


    @Nonnull
    public NodeSet<OWLNamedIndividual> getDifferentIndividuals(@Nonnull OWLNamedIndividual ind) {
        final OWLNamedIndividualNodeSet pn = new OWLNamedIndividualNodeSet();

        for (final OWLDifferentIndividualsAxiom a : o.getAxioms(AxiomType.DIFFERENT_INDIVIDUALS)) {
            if (a.getIndividuals().contains(ind)) {
                for (OWLIndividual e : a.getIndividuals()) {
                    if (e.isNamed()) {
                        pn.addEntity(e.asOWLNamedIndividual());
                    }
                }
            }
        }

        return pn;
    }

    @Nonnull
    public Node<OWLClass> getEquivalentClasses(@Nonnull OWLClassExpression ce) {
        final OWLClassNode pn = new OWLClassNode();

        for (final OWLEquivalentClassesAxiom a : o.getAxioms(AxiomType.EQUIVALENT_CLASSES)) {
            if (a.getClassExpressions().contains(ce)) {
                for (OWLClassExpression e : a.getClassExpressions()) {
                    if (!e.isAnonymous()) {
                        pn.add(e.asOWLClass());
                    }
                }
            }
        }

        return pn;
    }


    @Nonnull
    public Node<OWLDataProperty> getEquivalentDataProperties(@Nonnull OWLDataProperty pe) {
        final OWLDataPropertyNode pn = new OWLDataPropertyNode(pe);

        EntitySearcher.getEquivalentProperties(pe, o).filter(p -> !p.isAnonymous())
                      .forEach(p -> pn.add(p.asOWLDataProperty()));

        pn.add(pe);

        return pn;
    }


    @Nonnull
    public Node<OWLObjectPropertyExpression> getEquivalentObjectProperties(@Nonnull OWLObjectPropertyExpression pe) {
        final OWLObjectPropertyNode pn = new OWLObjectPropertyNode();

        for (final OWLEquivalentObjectPropertiesAxiom a : o.getAxioms(AxiomType.EQUIVALENT_OBJECT_PROPERTIES)) {
            if (a.getProperties().contains(pe)) {
                for (OWLObjectPropertyExpression e : a.getProperties()) {
                    if (!e.isAnonymous()) {
                        pn.add(e.asOWLObjectProperty());
                    }
                }
            }
        }

        return pn;
    }


    @Nonnull
    public IndividualNodeSetPolicy getIndividualNodeSetPolicy() {
        return IndividualNodeSetPolicy.BY_NAME;
    }


    @Nonnull
    public NodeSet<OWLNamedIndividual> getInstances(@Nonnull OWLClassExpression ce, boolean direct) {
        final OWLNamedIndividualNodeSet s = new OWLNamedIndividualNodeSet();

        for (final OWLClassAssertionAxiom a : o.getAxioms(AxiomType.CLASS_ASSERTION)) {
            if (a.getClassExpression().equals(ce) && a.getIndividual().isNamed()) {
                s.addEntity(a.getIndividual().asOWLNamedIndividual());
            }
        }
        return s;
    }


    @Nonnull
    public Node<OWLObjectPropertyExpression> getInverseObjectProperties(@Nonnull OWLObjectPropertyExpression pe) {
        final OWLObjectPropertyNode s = new OWLObjectPropertyNode();

        for (final OWLInverseObjectPropertiesAxiom a : o.getAxioms(AxiomType.INVERSE_OBJECT_PROPERTIES)) {
            if (a.getFirstProperty().equals(pe) && !a.getSecondProperty().isAnonymous()) {
                s.add(a.getSecondProperty().asOWLObjectProperty());
            } else if (a.getSecondProperty().equals(pe) && !a.getFirstProperty().isAnonymous()) {
                s.add(a.getFirstProperty().asOWLObjectProperty());
            }
        }
        return s;
    }


    @Nonnull
    public NodeSet<OWLClass> getObjectPropertyDomains(@Nonnull OWLObjectPropertyExpression pe, boolean direct) {
        final OWLClassNodeSet s = new OWLClassNodeSet();

        for (final OWLObjectPropertyDomainAxiom a : o.getAxioms(AxiomType.OBJECT_PROPERTY_DOMAIN)) {
            if (a.getProperty().equals(pe) && !a.getDomain().isAnonymous()) {
                s.addEntity(a.getDomain().asOWLClass());
            }
        }
        return s;
    }


    @Nonnull
    public NodeSet<OWLClass> getObjectPropertyRanges(
            @Nonnull OWLObjectPropertyExpression pe, boolean direct) {
        final OWLClassNodeSet s = new OWLClassNodeSet();

        for (final OWLObjectPropertyRangeAxiom a : o.getAxioms(AxiomType.OBJECT_PROPERTY_RANGE)) {
            if (a.getProperty().equals(pe) && !a.getRange().isAnonymous()) {
                s.addEntity(a.getRange().asOWLClass());
            }
        }
        return s;
    }


    @Nonnull
    public NodeSet<OWLNamedIndividual> getObjectPropertyValues(@Nonnull OWLNamedIndividual ind, @Nonnull
            OWLObjectPropertyExpression pe) {
        final OWLNamedIndividualNodeSet pn = new OWLNamedIndividualNodeSet();

        EntitySearcher.getObjectPropertyValues(ind, pe, o).filter(OWLIndividual::isNamed)
                      .forEach(i -> pn.addEntity(i.asOWLNamedIndividual()));

        return pn;
    }


    @Nonnull
    public Set<OWLAxiom> getPendingAxiomAdditions() {
        return Collections.emptySet();
    }


    @Nonnull
    public Set<OWLAxiom> getPendingAxiomRemovals() {
        return Collections.emptySet();
    }


    @Nonnull
    public List<OWLOntologyChange> getPendingChanges() {
        return Collections.emptyList();
    }


    @Nonnull
    public String getReasonerName() {
        return "identity reasoner";
    }


    @Nonnull
    public Version getReasonerVersion() {
        return new Version(0, 1, 0, 1);
    }


    @Nonnull
    public OWLOntology getRootOntology() {
        return o;
    }


    @Nonnull
    public Node<OWLNamedIndividual> getSameIndividuals(@Nonnull OWLNamedIndividual ind) {
        final OWLNamedIndividualNode pn = new OWLNamedIndividualNode();

        for (final OWLSameIndividualAxiom a : o.getAxioms(AxiomType.SAME_INDIVIDUAL)) {
            if (a.getIndividuals().contains(ind)) {
                for (OWLIndividual e : a.getIndividuals()) {
                    if (e.isNamed()) {
                        pn.add(e.asOWLNamedIndividual());
                    }
                }
            }
        }
        return pn;
    }


    @Nonnull
    public NodeSet<OWLClass> getSubClasses(@Nonnull OWLClassExpression ce, boolean direct) {
        final OWLClassNodeSet pn = new OWLClassNodeSet();

        for (final OWLSubClassOfAxiom a : o.getAxioms(AxiomType.SUBCLASS_OF)) {
            if (a.getSuperClass().equals(ce) && !a.getSubClass().isAnonymous()) {
                pn.addEntity(a.getSubClass().asOWLClass());
            }
        }
        return pn;
    }


    @Nonnull
    public NodeSet<OWLDataProperty> getSubDataProperties(@Nonnull OWLDataProperty pe, boolean direct) {
        final OWLDataPropertyNodeSet pn = new OWLDataPropertyNodeSet();

        for (final OWLSubDataPropertyOfAxiom a : o.getAxioms(AxiomType.SUB_DATA_PROPERTY)) {
            if (a.getSuperProperty().equals(pe) && !a.getSubProperty().isAnonymous()) {
                pn.addEntity(a.getSubProperty().asOWLDataProperty());
            }
        }
        return pn;
    }


    @Nonnull
    public NodeSet<OWLObjectPropertyExpression> getSubObjectProperties(
            @Nonnull OWLObjectPropertyExpression pe, boolean direct) {
        final OWLObjectPropertyNodeSet pn = new OWLObjectPropertyNodeSet();

        for (final OWLSubObjectPropertyOfAxiom a : o.getAxioms(AxiomType.SUB_OBJECT_PROPERTY)) {
            if (a.getSuperProperty().equals(pe) && !a.getSubProperty().isAnonymous()) {
                pn.addEntity(a.getSubProperty().asOWLObjectProperty());
            }
        }
        return pn;
    }


    @Nonnull
    public NodeSet<OWLClass> getSuperClasses(@Nonnull OWLClassExpression ce, boolean direct) {
        final OWLClassNodeSet pn = new OWLClassNodeSet();

        for (final OWLSubClassOfAxiom a : o.getAxioms(AxiomType.SUBCLASS_OF)) {
            if (a.getSubClass().equals(ce) && !a.getSuperClass().isAnonymous()) {
                pn.addEntity(a.getSuperClass().asOWLClass());
            }
        }
        return pn;
    }


    @Nonnull
    public NodeSet<OWLDataProperty> getSuperDataProperties(@Nonnull OWLDataProperty pe, boolean direct) {
        final OWLDataPropertyNodeSet pn = new OWLDataPropertyNodeSet();

        for (final OWLSubDataPropertyOfAxiom a : o.getAxioms(AxiomType.SUB_DATA_PROPERTY)) {
            if (a.getSubProperty().equals(pe) && !a.getSuperProperty().isAnonymous()) {
                pn.addEntity(a.getSuperProperty().asOWLDataProperty());
            }
        }
        return pn;
    }


    @Nonnull
    public NodeSet<OWLObjectPropertyExpression> getSuperObjectProperties(
            @Nonnull OWLObjectPropertyExpression pe, boolean direct) {
        final OWLObjectPropertyNodeSet pn = new OWLObjectPropertyNodeSet();

        for (final OWLSubObjectPropertyOfAxiom a : o.getAxioms(AxiomType.SUB_OBJECT_PROPERTY)) {
            if (a.getSubProperty().equals(pe) && !a.getSuperProperty().isAnonymous()) {
                pn.addEntity(a.getSuperProperty().asOWLObjectProperty());
            }
        }
        return pn;
    }


    public long getTimeOut() {
        return Long.MAX_VALUE;
    }


    @Nonnull
    public Node<OWLClass> getTopClassNode() {
        return new OWLClassNode(o.getOWLOntologyManager().getOWLDataFactory().getOWLThing());
    }


    @Nonnull
    public Node<OWLDataProperty> getTopDataPropertyNode() {
        return new OWLDataPropertyNode(o.getOWLOntologyManager().getOWLDataFactory().getOWLTopDataProperty());
    }


    @Nonnull
    public Node<OWLObjectPropertyExpression> getTopObjectPropertyNode() {
        return new OWLObjectPropertyNode(o.getOWLOntologyManager().getOWLDataFactory().getOWLTopObjectProperty());

    }


    @Nonnull
    public NodeSet<OWLClass> getTypes(@Nonnull OWLNamedIndividual ind, boolean direct) {
        final OWLClassNodeSet s = new OWLClassNodeSet();
        EntitySearcher.getTypes(ind, o).filter(ce -> !ce.isAnonymous()).forEach(ce -> s.addEntity(ce.asOWLClass()));

        return s;
    }


    @Nonnull
    public Node<OWLClass> getUnsatisfiableClasses() {
        return new OWLClassNode();
    }


    public void interrupt() {
        // do nothing
    }


    public boolean isConsistent() {
        return true;
    }


    public boolean isEntailed(@Nonnull OWLAxiom axiom) {
        return o.containsAxiom(axiom);
    }


    public boolean isEntailed(@Nonnull Set<? extends OWLAxiom> axioms) {
        for (final OWLAxiom a : axioms) {
            if (!o.containsAxiom(a)) {
                return false;
            }
        }
        return true;
    }


    public boolean isEntailmentCheckingSupported(@Nonnull AxiomType<?> axiomType) {
        return true;
    }


    public boolean isSatisfiable(@Nonnull OWLClassExpression classExpression) {
        return true;
    }


    @Nonnull
    public FreshEntityPolicy getFreshEntityPolicy() {
        return FreshEntityPolicy.DISALLOW;
    }


    @Nonnull
    public NodeSet<OWLClass> getDisjointClasses(@Nonnull OWLClassExpression arg0) {
        // TODO Auto-generated method stub
        return null;
    }


    @Nonnull
    public NodeSet<OWLDataProperty> getDisjointDataProperties(
            @Nonnull OWLDataPropertyExpression arg0) {
        // TODO Auto-generated method stub
        return null;
    }


    @Nonnull
    public NodeSet<OWLObjectPropertyExpression> getDisjointObjectProperties(
            @Nonnull OWLObjectPropertyExpression arg0) {
        // TODO Auto-generated method stub
        return null;
    }


    @Nonnull
    public Set<InferenceType> getPrecomputableInferenceTypes() {
        // TODO Auto-generated method stub
        return null;
    }


    public boolean isPrecomputed(@Nonnull InferenceType arg0) {
        // TODO Auto-generated method stub
        return false;
    }


    public void precomputeInferences(@Nonnull InferenceType... arg0) {
        // TODO Auto-generated method stub

    }
}
