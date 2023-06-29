/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.owlapi.query;

import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.*;
import org.semanticweb.owlapi.reasoner.impl.OWLClassNode;
import org.semanticweb.owlapi.reasoner.impl.OWLDataPropertyNode;
import org.semanticweb.owlapi.reasoner.impl.OWLObjectPropertyNode;
import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory;
import org.semanticweb.owlapi.util.Version;

import javax.annotation.Nonnull;
import java.util.List;
import java.util.Set;

import static uk.ac.manchester.cs.owl.owlapi.InternalizedEntities.*;

/**
 * A naive implementation of a no-op reasoner that just returns axioms asserted in the underlying ontology.
 * <p>
 * This implementation wraps a {@link org.semanticweb.owlapi.reasoner.structural.StructuralReasoner} and passes {@code
 * true} for direct whenever the called method allows it. All other calls are just forwarded to the underlying
 * reasoner.
 */
class NoOpReasoner implements OWLReasoner {

    private final OWLReasoner wrapped;

    public NoOpReasoner(OWLOntology rootOntology) {
        this.wrapped = createReasoner(rootOntology);
    }

    private static OWLReasoner createReasoner(OWLOntology rootOntology) {
        final StructuralReasonerFactory factory = new StructuralReasonerFactory();
        return factory.createReasoner(rootOntology);
    }

    @Nonnull
    public OWLOntology getRootOntology() {
        return wrapped.getRootOntology();
    }

    @Nonnull
    public Set<OWLAxiom> getPendingAxiomAdditions() {
        return wrapped.getPendingAxiomAdditions();
    }

    @Nonnull
    public Set<OWLAxiom> getPendingAxiomRemovals() {
        return wrapped.getPendingAxiomRemovals();
    }

    @Nonnull
    public List<OWLOntologyChange> getPendingChanges() {
        return wrapped.getPendingChanges();
    }

    @Nonnull
    public BufferingMode getBufferingMode() {
        return wrapped.getBufferingMode();
    }

    public long getTimeOut() {
        return wrapped.getTimeOut();
    }

    @Nonnull
    public Set<InferenceType> getPrecomputableInferenceTypes() {
        return wrapped.getPrecomputableInferenceTypes();
    }

    public boolean isPrecomputed(@Nonnull InferenceType inferenceType) {
        return wrapped.isPrecomputed(inferenceType);
    }

    public void precomputeInferences(
            @Nonnull InferenceType... inferenceTypes) throws ReasonerInterruptedException, TimeOutException, InconsistentOntologyException {
        wrapped.precomputeInferences(inferenceTypes);
    }

    public void interrupt() {
        wrapped.interrupt();
    }

    public void dispose() {
        wrapped.dispose();
    }

    public void flush() {
        wrapped.flush();
    }

    public boolean isConsistent() throws ReasonerInterruptedException, TimeOutException {
        return wrapped.isConsistent();
    }

    @Nonnull
    public NodeSet<OWLClass> getDataPropertyDomains(@Nonnull OWLDataProperty pe,
                                                    boolean direct) throws InconsistentOntologyException, ReasonerInterruptedException, TimeOutException {
        return wrapped.getDataPropertyDomains(pe, true);
    }

    @Nonnull
    public Set<OWLLiteral> getDataPropertyValues(@Nonnull OWLNamedIndividual ind,
                                                 @Nonnull OWLDataProperty pe) throws InconsistentOntologyException, ReasonerInterruptedException, TimeOutException {
        return wrapped.getDataPropertyValues(ind, pe);
    }

    @Nonnull
    public Node<OWLClass> getEquivalentClasses(
            @Nonnull OWLClassExpression ce) throws InconsistentOntologyException, ClassExpressionNotInProfileException, ReasonerInterruptedException, TimeOutException {
        return wrapped.getEquivalentClasses(ce);
    }

    @Nonnull
    public Node<OWLDataProperty> getEquivalentDataProperties(
            @Nonnull OWLDataProperty pe) throws InconsistentOntologyException, ReasonerInterruptedException, TimeOutException {
        return wrapped.getEquivalentDataProperties(pe);
    }

    @Nonnull
    public Node<OWLObjectPropertyExpression> getEquivalentObjectProperties(
            @Nonnull OWLObjectPropertyExpression pe) throws InconsistentOntologyException, ReasonerInterruptedException, TimeOutException {
        return wrapped.getEquivalentObjectProperties(pe);
    }

    @Nonnull
    public NodeSet<OWLNamedIndividual> getInstances(@Nonnull OWLClassExpression ce,
                                                    boolean direct) throws InconsistentOntologyException, ClassExpressionNotInProfileException, ReasonerInterruptedException, TimeOutException {
        return wrapped.getInstances(ce, true);
    }

    @Nonnull
    public Node<OWLObjectPropertyExpression> getInverseObjectProperties(
            @Nonnull OWLObjectPropertyExpression pe) throws InconsistentOntologyException, ReasonerInterruptedException, TimeOutException {
        return wrapped.getInverseObjectProperties(pe);
    }

    @Nonnull
    public NodeSet<OWLClass> getObjectPropertyDomains(@Nonnull OWLObjectPropertyExpression pe,
                                                      boolean direct) throws InconsistentOntologyException, ReasonerInterruptedException, TimeOutException {
        return wrapped.getObjectPropertyDomains(pe, true);
    }

    @Nonnull
    public NodeSet<OWLClass> getObjectPropertyRanges(@Nonnull OWLObjectPropertyExpression pe,
                                                     boolean direct) throws InconsistentOntologyException, ReasonerInterruptedException, TimeOutException {
        return wrapped.getObjectPropertyRanges(pe, true);
    }

    @Nonnull
    public NodeSet<OWLNamedIndividual> getObjectPropertyValues(@Nonnull OWLNamedIndividual ind,
                                                               @Nonnull OWLObjectPropertyExpression pe) throws InconsistentOntologyException, ReasonerInterruptedException, TimeOutException {
        return wrapped.getObjectPropertyValues(ind, pe);
    }


    @Nonnull
    public Node<OWLNamedIndividual> getSameIndividuals(
            @Nonnull OWLNamedIndividual ind) throws InconsistentOntologyException, ReasonerInterruptedException, TimeOutException {
        return wrapped.getSameIndividuals(ind);
    }

    @Nonnull
    public NodeSet<OWLClass> getSubClasses(@Nonnull OWLClassExpression ce,
                                           boolean direct) throws InconsistentOntologyException, ClassExpressionNotInProfileException, ReasonerInterruptedException, TimeOutException {
        return wrapped.getSubClasses(ce, true);
    }

    @Nonnull
    public NodeSet<OWLDataProperty> getSubDataProperties(@Nonnull OWLDataProperty pe,
                                                         boolean direct) throws InconsistentOntologyException, ReasonerInterruptedException, TimeOutException {
        return wrapped.getSubDataProperties(pe, true);
    }

    @Nonnull
    public NodeSet<OWLObjectPropertyExpression> getSubObjectProperties(@Nonnull OWLObjectPropertyExpression pe,
                                                                       boolean direct) throws InconsistentOntologyException, ReasonerInterruptedException, TimeOutException {
        return wrapped.getSubObjectProperties(pe, true);
    }

    @Nonnull
    public NodeSet<OWLClass> getSuperClasses(@Nonnull OWLClassExpression ce,
                                             boolean direct) throws InconsistentOntologyException, ClassExpressionNotInProfileException, ReasonerInterruptedException, TimeOutException {
        return wrapped.getSuperClasses(ce, true);
    }

    @Nonnull
    public NodeSet<OWLDataProperty> getSuperDataProperties(@Nonnull OWLDataProperty pe,
                                                           boolean direct) throws InconsistentOntologyException, ReasonerInterruptedException, TimeOutException {
        return wrapped.getSuperDataProperties(pe, true);
    }

    @Nonnull
    public NodeSet<OWLObjectPropertyExpression> getSuperObjectProperties(@Nonnull OWLObjectPropertyExpression pe,
                                                                         boolean direct) throws InconsistentOntologyException, ReasonerInterruptedException, TimeOutException {
        return wrapped.getSuperObjectProperties(pe, true);
    }

    @Nonnull
    public NodeSet<OWLClass> getTypes(@Nonnull OWLNamedIndividual ind,
                                      boolean direct) throws InconsistentOntologyException, ReasonerInterruptedException, TimeOutException {
        return wrapped.getTypes(ind, true);
    }

    @Nonnull
    public Node<OWLClass> getUnsatisfiableClasses() throws ReasonerInterruptedException, TimeOutException {
        return wrapped.getUnsatisfiableClasses();
    }

    public boolean isEntailed(
            @Nonnull OWLAxiom axiom) throws ReasonerInterruptedException, UnsupportedEntailmentTypeException, TimeOutException, AxiomNotInProfileException, InconsistentOntologyException {
        return wrapped.isEntailed(axiom);
    }

    public boolean isEntailed(
            @Nonnull Set<? extends OWLAxiom> axioms) throws ReasonerInterruptedException, UnsupportedEntailmentTypeException, TimeOutException, AxiomNotInProfileException, InconsistentOntologyException {
        return wrapped.isEntailed(axioms);
    }

    public boolean isEntailmentCheckingSupported(@Nonnull AxiomType<?> axiomType) {
        return wrapped.isEntailmentCheckingSupported(axiomType);
    }

    public boolean isSatisfiable(
            @Nonnull OWLClassExpression ce) throws ReasonerInterruptedException, TimeOutException, ClassExpressionNotInProfileException, InconsistentOntologyException {
        return wrapped.isSatisfiable(ce);
    }

    @Nonnull
    public Node<OWLClass> getBottomClassNode() {
        return new OWLClassNode(OWL_NOTHING);
    }

    @Nonnull
    public Node<OWLDataProperty> getBottomDataPropertyNode() {
        return new OWLDataPropertyNode(OWL_BOTTOM_DATA_PROPERTY);
    }

    @Nonnull
    public Node<OWLObjectPropertyExpression> getBottomObjectPropertyNode() {
        return new OWLObjectPropertyNode(OWL_BOTTOM_OBJECT_PROPERTY);
    }

    @Nonnull
    public NodeSet<OWLNamedIndividual> getDifferentIndividuals(
            @Nonnull OWLNamedIndividual ind) throws InconsistentOntologyException, ReasonerInterruptedException, TimeOutException {
        return wrapped.getDifferentIndividuals(ind);
    }

    @Nonnull
    public NodeSet<OWLClass> getDisjointClasses(@Nonnull OWLClassExpression ce) {
        return wrapped.getDisjointClasses(ce);
    }

    @Nonnull
    public NodeSet<OWLDataProperty> getDisjointDataProperties(
            @Nonnull OWLDataPropertyExpression pe) throws InconsistentOntologyException, ReasonerInterruptedException, TimeOutException {
        return wrapped.getDisjointDataProperties(pe);
    }

    @Nonnull
    public NodeSet<OWLObjectPropertyExpression> getDisjointObjectProperties(
            @Nonnull OWLObjectPropertyExpression pe) throws InconsistentOntologyException, ReasonerInterruptedException, TimeOutException {
        return wrapped.getDisjointObjectProperties(pe);
    }

    @Nonnull
    public IndividualNodeSetPolicy getIndividualNodeSetPolicy() {
        return wrapped.getIndividualNodeSetPolicy();
    }

    @Nonnull
    public String getReasonerName() {
        return "Owlapi Driver No-op Reasoner";
    }

    @Nonnull
    public Version getReasonerVersion() {
        return new Version(1, 0, 0, 0);
    }

    @Nonnull
    public Node<OWLClass> getTopClassNode() {
        return new OWLClassNode(OWL_THING);
    }

    @Nonnull
    public Node<OWLDataProperty> getTopDataPropertyNode() {
        return new OWLDataPropertyNode(OWL_TOP_DATA_PROPERTY);
    }

    @Nonnull
    public Node<OWLObjectPropertyExpression> getTopObjectPropertyNode() {
        return new OWLObjectPropertyNode(OWL_TOP_OBJECT_PROPERTY);
    }

    @Nonnull
    public FreshEntityPolicy getFreshEntityPolicy() {
        return wrapped.getFreshEntityPolicy();
    }
}
