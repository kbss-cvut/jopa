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

package cz.cvut.kbss.jopa.owlapi.identityreasoner;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.semanticweb.owlapi.model.AxiomType;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassAssertionAxiom;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLDataPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLDataPropertyDomainAxiom;
import org.semanticweb.owlapi.model.OWLDataPropertyExpression;
import org.semanticweb.owlapi.model.OWLDifferentIndividualsAxiom;
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom;
import org.semanticweb.owlapi.model.OWLEquivalentObjectPropertiesAxiom;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLInverseObjectPropertiesAxiom;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectPropertyDomainAxiom;
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression;
import org.semanticweb.owlapi.model.OWLObjectPropertyRangeAxiom;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChange;
import org.semanticweb.owlapi.model.OWLSameIndividualAxiom;
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom;
import org.semanticweb.owlapi.model.OWLSubDataPropertyOfAxiom;
import org.semanticweb.owlapi.model.OWLSubObjectPropertyOfAxiom;
import org.semanticweb.owlapi.reasoner.AxiomNotInProfileException;
import org.semanticweb.owlapi.reasoner.BufferingMode;
import org.semanticweb.owlapi.reasoner.ClassExpressionNotInProfileException;
import org.semanticweb.owlapi.reasoner.FreshEntitiesException;
import org.semanticweb.owlapi.reasoner.FreshEntityPolicy;
import org.semanticweb.owlapi.reasoner.InconsistentOntologyException;
import org.semanticweb.owlapi.reasoner.IndividualNodeSetPolicy;
import org.semanticweb.owlapi.reasoner.InferenceType;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.NodeSet;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.ReasonerInterruptedException;
import org.semanticweb.owlapi.reasoner.TimeOutException;
import org.semanticweb.owlapi.reasoner.UnsupportedEntailmentTypeException;
import org.semanticweb.owlapi.reasoner.impl.OWLClassNode;
import org.semanticweb.owlapi.reasoner.impl.OWLClassNodeSet;
import org.semanticweb.owlapi.reasoner.impl.OWLDataPropertyNode;
import org.semanticweb.owlapi.reasoner.impl.OWLDataPropertyNodeSet;
import org.semanticweb.owlapi.reasoner.impl.OWLNamedIndividualNode;
import org.semanticweb.owlapi.reasoner.impl.OWLNamedIndividualNodeSet;
import org.semanticweb.owlapi.reasoner.impl.OWLObjectPropertyNode;
import org.semanticweb.owlapi.reasoner.impl.OWLObjectPropertyNodeSet;
import org.semanticweb.owlapi.util.Version;

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

	
	public Node<OWLClass> getBottomClassNode() {
		return new OWLClassNode(o.getOWLOntologyManager().getOWLDataFactory()
				.getOWLNothing());
	}

	
	public Node<OWLDataProperty> getBottomDataPropertyNode() {
		return new OWLDataPropertyNode(o.getOWLOntologyManager()
				.getOWLDataFactory().getOWLBottomDataProperty());
	}

	
	public Node<OWLObjectPropertyExpression> getBottomObjectPropertyNode() {
		return new OWLObjectPropertyNode(o.getOWLOntologyManager()
				.getOWLDataFactory().getOWLBottomObjectProperty());
	}

	
	public BufferingMode getBufferingMode() {
		return BufferingMode.NON_BUFFERING;
	}

	
	public NodeSet<OWLClass> getDataPropertyDomains(OWLDataProperty pe,
			boolean direct) throws InconsistentOntologyException,
			ReasonerInterruptedException, TimeOutException {
		final OWLClassNodeSet s = new OWLClassNodeSet();

		for (final OWLDataPropertyDomainAxiom a : o
				.getAxioms(AxiomType.DATA_PROPERTY_DOMAIN)) {
			if (a.getProperty().equals(pe) && !a.getDomain().isAnonymous()) {
				s.addEntity(a.getDomain().asOWLClass());
			}
		}

		return s;
	}

	
	public Set<OWLLiteral> getDataPropertyValues(OWLNamedIndividual ind,
			OWLDataProperty pe) throws InconsistentOntologyException,
			ReasonerInterruptedException, TimeOutException {
		final Set<OWLLiteral> literals = new HashSet<OWLLiteral>();

		for (final OWLDataPropertyAssertionAxiom a : o
				.getAxioms(AxiomType.DATA_PROPERTY_ASSERTION)) {
			if (a.getProperty().equals(pe) && a.getSubject().equals(ind)) {
				// if (a.getObject().isNamed()) {
				// pn.addEntity(a.getObject().asOWLNamedIndividual());
				// }
				literals.add(a.getObject());
			}
		}

		return literals;
	}

	
	public NodeSet<OWLNamedIndividual> getDifferentIndividuals(
			OWLNamedIndividual ind) throws InconsistentOntologyException,
			ReasonerInterruptedException, TimeOutException {
		final OWLNamedIndividualNodeSet pn = new OWLNamedIndividualNodeSet();

		for (final OWLDifferentIndividualsAxiom a : o
				.getAxioms(AxiomType.DIFFERENT_INDIVIDUALS)) {
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

//	
//	public NodeSet<OWLClass> getDisjointClasses(OWLClassExpression ce,
//			boolean direct) {
//		final OWLClassNodeSet pn = new OWLClassNodeSet();
//
//		for (final OWLDisjointClassesAxiom a : o
//				.getAxioms(AxiomType.DISJOINT_CLASSES)) {
//			if (a.getClassExpressions().contains(ce)) {
//				for (OWLClassExpression e : a.getClassExpressions()) {
//					if (!e.isAnonymous()) {
//						pn.addEntity(e.asOWLClass());
//					}
//				}
//			}
//		}
//
//		return pn;
//	}
//
//	
//	public NodeSet<OWLDataProperty> getDisjointDataProperties(
//			OWLDataPropertyExpression pe, boolean direct)
//			throws InconsistentOntologyException, ReasonerInterruptedException,
//			TimeOutException {
//		final OWLDataPropertyNodeSet pn = new OWLDataPropertyNodeSet();
//
//		for (final OWLDisjointDataPropertiesAxiom a : o
//				.getAxioms(AxiomType.DISJOINT_DATA_PROPERTIES)) {
//			if (a.getProperties().contains(pe)) {
//				for (OWLDataPropertyExpression e : a.getProperties()) {
//					if (!e.isAnonymous()) {
//						pn.addEntity(e.asOWLDataProperty());
//					}
//				}
//			}
//		}
//
//		return pn;
//	}
//
//	
//	public NodeSet<OWLObjectProperty> getDisjointObjectProperties(
//			OWLObjectPropertyExpression pe, boolean direct)
//			throws InconsistentOntologyException, ReasonerInterruptedException,
//			TimeOutException {
//		final OWLObjectPropertyNodeSet pn = new OWLObjectPropertyNodeSet();
//
//		for (final OWLDisjointObjectPropertiesAxiom a : o
//				.getAxioms(AxiomType.DISJOINT_OBJECT_PROPERTIES)) {
//			if (a.getProperties().contains(pe)) {
//				for (OWLObjectPropertyExpression e : a.getProperties()) {
//					if (!e.isAnonymous()) {
//						pn.addEntity(e.asOWLObjectProperty());
//					}
//				}
//			}
//		}
//
//		return pn;
//	}

	
	public Node<OWLClass> getEquivalentClasses(OWLClassExpression ce)
			throws InconsistentOntologyException,
			ClassExpressionNotInProfileException, ReasonerInterruptedException,
			TimeOutException {
		final OWLClassNode pn = new OWLClassNode();

		for (final OWLEquivalentClassesAxiom a : o
				.getAxioms(AxiomType.EQUIVALENT_CLASSES)) {
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

	
	public Node<OWLDataProperty> getEquivalentDataProperties(OWLDataProperty pe)
			throws InconsistentOntologyException, ReasonerInterruptedException,
			TimeOutException {
		final OWLDataPropertyNode pn = new OWLDataPropertyNode(pe);

		for (final OWLDataPropertyExpression p : pe.getEquivalentProperties(o)) {
			if (!p.isAnonymous()) {
				pn.add(p.asOWLDataProperty());
			}
		}

		pn.add(pe);

		return pn;
	}

	
	public Node<OWLObjectPropertyExpression> getEquivalentObjectProperties(
			OWLObjectPropertyExpression pe)
			throws InconsistentOntologyException, ReasonerInterruptedException,
			TimeOutException {
		final OWLObjectPropertyNode pn = new OWLObjectPropertyNode();

		for (final OWLEquivalentObjectPropertiesAxiom a : o
				.getAxioms(AxiomType.EQUIVALENT_OBJECT_PROPERTIES)) {
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

	
	public IndividualNodeSetPolicy getIndividualNodeSetPolicy() {
		return IndividualNodeSetPolicy.BY_NAME;
	}

	
	public NodeSet<OWLNamedIndividual> getInstances(OWLClassExpression ce,
			boolean direct) throws InconsistentOntologyException,
			ClassExpressionNotInProfileException, ReasonerInterruptedException,
			TimeOutException {
		final OWLNamedIndividualNodeSet s = new OWLNamedIndividualNodeSet();

		for (final OWLClassAssertionAxiom a : o
				.getAxioms(AxiomType.CLASS_ASSERTION)) {
			if (a.getClassExpression().equals(ce)
					&& a.getIndividual().isNamed()) {
				s.addEntity(a.getIndividual().asOWLNamedIndividual());
			}
		}

		return s;
	}

	
	public Node<OWLObjectPropertyExpression> getInverseObjectProperties(
			OWLObjectPropertyExpression pe)
			throws InconsistentOntologyException, ReasonerInterruptedException,
			TimeOutException {
		final OWLObjectPropertyNode s = new OWLObjectPropertyNode();

		for (final OWLInverseObjectPropertiesAxiom a : o
				.getAxioms(AxiomType.INVERSE_OBJECT_PROPERTIES)) {
			if (a.getFirstProperty().equals(pe)) {
				if (!a.getSecondProperty().isAnonymous()) {
					s.add(a.getSecondProperty().asOWLObjectProperty());
				}
			} else if (a.getSecondProperty().equals(pe)) {
				if (!a.getFirstProperty().isAnonymous()) {
					s.add(a.getFirstProperty().asOWLObjectProperty());
				}
			}
		}

		return s;
	}

	
	public NodeSet<OWLClass> getObjectPropertyDomains(
			OWLObjectPropertyExpression pe, boolean direct)
			throws InconsistentOntologyException, ReasonerInterruptedException,
			TimeOutException {
		final OWLClassNodeSet s = new OWLClassNodeSet();

		for (final OWLObjectPropertyDomainAxiom a : o
				.getAxioms(AxiomType.OBJECT_PROPERTY_DOMAIN)) {
			if (a.getProperty().equals(pe) && !a.getDomain().isAnonymous()) {
				s.addEntity(a.getDomain().asOWLClass());
			}
		}

		return s;
	}

	
	public NodeSet<OWLClass> getObjectPropertyRanges(
			OWLObjectPropertyExpression pe, boolean direct)
			throws InconsistentOntologyException, ReasonerInterruptedException,
			TimeOutException {
		final OWLClassNodeSet s = new OWLClassNodeSet();

		for (final OWLObjectPropertyRangeAxiom a : o
				.getAxioms(AxiomType.OBJECT_PROPERTY_RANGE)) {
			if (a.getProperty().equals(pe) && !a.getRange().isAnonymous()) {
				s.addEntity(a.getRange().asOWLClass());
			}
		}

		return s;
	}

	
	public NodeSet<OWLNamedIndividual> getObjectPropertyValues(
			OWLNamedIndividual ind, OWLObjectPropertyExpression pe)
			throws InconsistentOntologyException, ReasonerInterruptedException,
			TimeOutException {
		final OWLNamedIndividualNodeSet pn = new OWLNamedIndividualNodeSet();

		for (final OWLIndividual i : ind.getObjectPropertyValues(pe, o)) {
			if (i.isNamed()) {
				pn.addEntity(i.asOWLNamedIndividual());
			}
		}
		//		
		// o.getOWLOntologyManager().getOWLDataFactory().getOWLNamedIndividual(iri)
		//		
		// for (final OWLObjectPropertyAssertionAxiom a : o
		// .getAxioms(AxiomType.OBJECT_PROPERTY_ASSERTION)) {
		// if (a.getProperty().equals(pe) && a.getSubject().equals(ind)) {
		// if (a.getObject().isNamed()) {
		// pn.addEntity(a.getObject().asOWLNamedIndividual());
		// }
		// }
		// }

		return pn;
	}

	
	public Set<OWLAxiom> getPendingAxiomAdditions() {
		return Collections.emptySet();
	}

	
	public Set<OWLAxiom> getPendingAxiomRemovals() {
		return Collections.emptySet();
	}

	
	public List<OWLOntologyChange> getPendingChanges() {
		return Collections.emptyList();
	}

	
	public String getReasonerName() {
		return "identity reasoner";
	}

	
	public Version getReasonerVersion() {
		return new Version(0, 1, 0, 1);
	}

	
	public OWLOntology getRootOntology() {
		return o;
	}

	
	public Node<OWLNamedIndividual> getSameIndividuals(OWLNamedIndividual ind)
			throws InconsistentOntologyException, ReasonerInterruptedException,
			TimeOutException {
		final OWLNamedIndividualNode pn = new OWLNamedIndividualNode();

		for (final OWLSameIndividualAxiom a : o
				.getAxioms(AxiomType.SAME_INDIVIDUAL)) {
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

	
	public NodeSet<OWLClass> getSubClasses(OWLClassExpression ce, boolean direct) {
		final OWLClassNodeSet pn = new OWLClassNodeSet();

		for (final OWLSubClassOfAxiom a : o.getAxioms(AxiomType.SUBCLASS_OF)) {
			if (a.getSuperClass().equals(ce)) {
				if (!a.getSubClass().isAnonymous()) {
					pn.addEntity(a.getSubClass().asOWLClass());
				}
			}
		}

		return pn;
	}

	
	public NodeSet<OWLDataProperty> getSubDataProperties(OWLDataProperty pe,
			boolean direct) throws InconsistentOntologyException,
			ReasonerInterruptedException, TimeOutException {
		final OWLDataPropertyNodeSet pn = new OWLDataPropertyNodeSet();

		for (final OWLSubDataPropertyOfAxiom a : o
				.getAxioms(AxiomType.SUB_DATA_PROPERTY)) {
			if (a.getSuperProperty().equals(pe)) {
				if (!a.getSubProperty().isAnonymous()) {
					pn.addEntity(a.getSubProperty().asOWLDataProperty());
				}
			}
		}

		return pn;
	}

	
	public NodeSet<OWLObjectPropertyExpression> getSubObjectProperties(
			OWLObjectPropertyExpression pe, boolean direct)
			throws InconsistentOntologyException, ReasonerInterruptedException,
			TimeOutException {
		final OWLObjectPropertyNodeSet pn = new OWLObjectPropertyNodeSet();

		for (final OWLSubObjectPropertyOfAxiom a : o
				.getAxioms(AxiomType.SUB_OBJECT_PROPERTY)) {
			if (a.getSuperProperty().equals(pe)) {
				if (!a.getSubProperty().isAnonymous()) {
					pn.addEntity(a.getSubProperty().asOWLObjectProperty());
				}
			}
		}

		return pn;
	}

	
	public NodeSet<OWLClass> getSuperClasses(OWLClassExpression ce,
			boolean direct) throws InconsistentOntologyException,
			ClassExpressionNotInProfileException, ReasonerInterruptedException,
			TimeOutException {
		final OWLClassNodeSet pn = new OWLClassNodeSet();

		for (final OWLSubClassOfAxiom a : o.getAxioms(AxiomType.SUBCLASS_OF)) {
			if (a.getSubClass().equals(ce)) {
				if (!a.getSuperClass().isAnonymous()) {
					pn.addEntity(a.getSuperClass().asOWLClass());
				}
			}
		}

		return pn;
	}

	
	public NodeSet<OWLDataProperty> getSuperDataProperties(OWLDataProperty pe,
			boolean direct) throws InconsistentOntologyException,
			ReasonerInterruptedException, TimeOutException {
		final OWLDataPropertyNodeSet pn = new OWLDataPropertyNodeSet();

		for (final OWLSubDataPropertyOfAxiom a : o
				.getAxioms(AxiomType.SUB_DATA_PROPERTY)) {
			if (a.getSubProperty().equals(pe)) {
				if (!a.getSuperProperty().isAnonymous()) {
					pn.addEntity(a.getSuperProperty().asOWLDataProperty());
				}
			}
		}

		return pn;
	}

	
	public NodeSet<OWLObjectPropertyExpression> getSuperObjectProperties(
			OWLObjectPropertyExpression pe, boolean direct)
			throws InconsistentOntologyException, ReasonerInterruptedException,
			TimeOutException {
		final OWLObjectPropertyNodeSet pn = new OWLObjectPropertyNodeSet();

		for (final OWLSubObjectPropertyOfAxiom a : o
				.getAxioms(AxiomType.SUB_OBJECT_PROPERTY)) {
			if (a.getSubProperty().equals(pe)) {
				if (!a.getSuperProperty().isAnonymous()) {
					pn.addEntity(a.getSuperProperty().asOWLObjectProperty());
				}
			}
		}

		return pn;
	}

	
	public long getTimeOut() {
		return Long.MAX_VALUE;
	}

	
	public Node<OWLClass> getTopClassNode() {
		return new OWLClassNode(o.getOWLOntologyManager().getOWLDataFactory()
				.getOWLThing());
	}

	
	public Node<OWLDataProperty> getTopDataPropertyNode() {
		return new OWLDataPropertyNode(o.getOWLOntologyManager()
				.getOWLDataFactory().getOWLTopDataProperty());
	}

	
	public Node<OWLObjectPropertyExpression> getTopObjectPropertyNode() {
		return new OWLObjectPropertyNode(o.getOWLOntologyManager()
				.getOWLDataFactory().getOWLTopObjectProperty());

	}

	
	public NodeSet<OWLClass> getTypes(OWLNamedIndividual ind, boolean direct)
			throws InconsistentOntologyException, ReasonerInterruptedException,
			TimeOutException {
		final OWLClassNodeSet s = new OWLClassNodeSet();

		// for (final OWLClassAssertionAxiom a : o
		// .getAxioms(AxiomType.CLASS_ASSERTION)) {
		// if (a.getIndividual().equals(ce)
		// && a.getIndividual().isNamed()) {
		// s.addEntity(a.getIndividual().asOWLNamedIndividual());
		// }
		// }

		for (final OWLClassExpression ce : ind.getTypes(o)) {
			if (!ce.isAnonymous()) {
				s.addEntity(ce.asOWLClass());
			}
		}

		return s;
	}

	
	public Node<OWLClass> getUnsatisfiableClasses()
			throws ReasonerInterruptedException, TimeOutException {
		return new OWLClassNode();
	}

	
	public void interrupt() {
		// do nothing
	}

	
	public boolean isConsistent() throws ReasonerInterruptedException,
			TimeOutException {
		return true;
	}

	
	public boolean isEntailed(OWLAxiom axiom)
			throws ReasonerInterruptedException,
			UnsupportedEntailmentTypeException, TimeOutException,
			AxiomNotInProfileException {
		return o.containsAxiom(axiom);
	}

	
	public boolean isEntailed(Set<? extends OWLAxiom> axioms)
			throws ReasonerInterruptedException,
			UnsupportedEntailmentTypeException, TimeOutException,
			AxiomNotInProfileException {
		for (final OWLAxiom a : axioms) {
			if (!o.containsAxiom(a)) {
				return false;
			}
		}
		return true;
	}

	
	public boolean isEntailmentCheckingSupported(AxiomType<?> axiomType) {
		return true;
	}

	
	public boolean isSatisfiable(OWLClassExpression classExpression)
			throws ReasonerInterruptedException, TimeOutException,
			ClassExpressionNotInProfileException, InconsistentOntologyException {
		return true;
	}

	
	public FreshEntityPolicy getFreshEntityPolicy() {
		return FreshEntityPolicy.DISALLOW;
	}

	
	public NodeSet<OWLClass> getDisjointClasses(OWLClassExpression arg0)
			throws ReasonerInterruptedException, TimeOutException,
			FreshEntitiesException, InconsistentOntologyException {
		// TODO Auto-generated method stub
		return null;
	}

	
	public NodeSet<OWLDataProperty> getDisjointDataProperties(
			OWLDataPropertyExpression arg0)
			throws InconsistentOntologyException, FreshEntitiesException,
			ReasonerInterruptedException, TimeOutException {
		// TODO Auto-generated method stub
		return null;
	}

	
	public NodeSet<OWLObjectPropertyExpression> getDisjointObjectProperties(
			OWLObjectPropertyExpression arg0)
			throws InconsistentOntologyException, FreshEntitiesException,
			ReasonerInterruptedException, TimeOutException {
		// TODO Auto-generated method stub
		return null;
	}

	
	public Set<InferenceType> getPrecomputableInferenceTypes() {
		// TODO Auto-generated method stub
		return null;
	}

	
	public boolean isPrecomputed(InferenceType arg0) {
		// TODO Auto-generated method stub
		return false;
	}

	
	public void precomputeInferences(InferenceType... arg0)
			throws ReasonerInterruptedException, TimeOutException,
			InconsistentOntologyException {
		// TODO Auto-generated method stub
		
	}
}
