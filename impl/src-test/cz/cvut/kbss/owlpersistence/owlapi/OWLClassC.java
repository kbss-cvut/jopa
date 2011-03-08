package cz.cvut.kbss.owlpersistence.owlapi;

import java.net.URI;
import java.util.List;

import cz.cvut.kbss.owlpersistence.model.annotations.Id;
import cz.cvut.kbss.owlpersistence.model.annotations.OWLClass;
import cz.cvut.kbss.owlpersistence.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.owlpersistence.model.annotations.Sequence;
import cz.cvut.kbss.owlpersistence.model.annotations.SequenceType;

@OWLClass(iri = "http://new.owl#OWLClassC")
public class OWLClassC {

	@Id
	private URI uri;

	@Sequence
	@OWLObjectProperty(iri = "http://B-hasReferencedSequence")
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

}
