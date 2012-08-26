package cz.cvut.kbss.jopa.owlapi;

import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;

@OWLClass(iri = "http://OWLClassF")
public class OWLClassF extends OWLClassE {

	@OWLDataProperty(iri = "http://F-secondStringAttribute", inferred = true)
	private String secondStringAttribute;

	public String getSecondStringAttribute() {
		return secondStringAttribute;
	}

	public void setSecondStringAttribute(String secondStringAttribute) {
		this.secondStringAttribute = secondStringAttribute;
	}
}
