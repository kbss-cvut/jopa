package cz.cvut.kbss.owlpersistence.owlapi;

import java.net.URI;
import java.util.List;

import cz.cvut.kbss.owlpersistence.Id;
import cz.cvut.kbss.owlpersistence.OWLClass;
import cz.cvut.kbss.owlpersistence.OWLObjectProperty;
import cz.cvut.kbss.owlpersistence.OWLSequence;
import cz.cvut.kbss.owlpersistence.OWLSequenceType;

@OWLClass(uri = "http://OWLClassC")
public class OWLClassC {

	@Id
	private URI uri;

	@OWLSequence
	@OWLObjectProperty(uri="http://B-hasReferencedSequence")
	private List<OWLClassA> referencedList;

	@OWLSequence(type = OWLSequenceType.simple, ObjectPropertyHasNextURI = "http://B-hasSimpleNext")
	@OWLObjectProperty(uri = "http://B-hasSimpleSequence")
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

}
