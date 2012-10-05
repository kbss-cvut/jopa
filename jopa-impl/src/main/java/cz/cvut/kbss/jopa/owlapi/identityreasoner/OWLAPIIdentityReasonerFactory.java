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

import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.OWLReasonerConfiguration;
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory;

public class OWLAPIIdentityReasonerFactory implements OWLReasonerFactory {

	
	public OWLReasoner createNonBufferingReasoner(OWLOntology ontology) {
		return createNonBufferingReasoner(ontology, null);
	}

	
	public OWLReasoner createNonBufferingReasoner(OWLOntology ontology,
			OWLReasonerConfiguration config) {
		return new OWLAPIIdentityReasoner(ontology);
	}

	
	public OWLReasoner createReasoner(OWLOntology ontology) {
		return createReasoner(ontology, null);
	}

	
	public OWLReasoner createReasoner(OWLOntology ontology,
			OWLReasonerConfiguration config) {
		return new OWLAPIIdentityReasoner(ontology);
	}

	
	public String getReasonerName() {
		return "Identity reasoner";
	}
}
