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

package cz.cvut.kbss.jopa.owlapi;

import java.net.URI;
import java.util.List;

import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.annotations.Sequence;
import cz.cvut.kbss.jopa.model.annotations.SequenceType;

@OWLClass(iri = "http://new.owl#OWLClassC")
public class OWLClassC {

	@Id
	private URI uri;

	@Sequence
	@OWLObjectProperty(iri = "http://B-hasReferencedSequence", fetch = FetchType.EAGER)
	private List<OWLClassA> referencedList;

	@Sequence(type = SequenceType.simple, ObjectPropertyHasNextIRI = "http://B-hasSimpleNext")
	@OWLObjectProperty(iri = "http://B-hasSimpleSequence")
	private List<OWLClassA> simplelist;

	public void setUri(URI uri) {
		this.uri = uri;
	}

	public URI getUri() {
		return uri;
	}

	public void setReferencedList(List<OWLClassA> list) {
		this.referencedList = list;
	}

	public List<OWLClassA> getReferencedList() {
		return referencedList;
	}

	public void setSimpleList(List<OWLClassA> simplelist) {
		this.simplelist = simplelist;
	}

	public List<OWLClassA> getSimpleList() {
		return simplelist;
	}

//	@Override
//	public String toString() {
//		String out = "OWLClassC: uri = " + uri;
//		if (referencedList != null) {
//			out += ", referencedList = {" + referencedList.toString() + "}";
//		}
//		if (simplelist != null) {
//			out += ", simpleList = {" + simplelist.toString() + "}";
//		}
//		return out;
//	}
}
