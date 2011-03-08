package cz.cvut.kbss.owlpersistence.model;

import cz.cvut.kbss.owlpersistence.model.annotations.Id;
import cz.cvut.kbss.owlpersistence.model.annotations.OWLClass;

@OWLClass(iri = "http://www.w3.org/2002/07/owl#Thing")
public class Thing {

	@Id
	private String id;

	public void setId(String id) {
		this.id = id;
	}

	public String getId() {
		return id;
	}
}