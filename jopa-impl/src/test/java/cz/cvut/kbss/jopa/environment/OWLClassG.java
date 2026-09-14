/*
 * JOPA
 * Copyright (C) 2026 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.environment;

import cz.cvut.kbss.jopa.environment.utils.HasUri;
import cz.cvut.kbss.jopa.model.annotations.CascadeType;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.vocabulary.RDFS;

import java.lang.reflect.Field;
import java.net.URI;

@OWLClass(iri = Vocabulary.c_OwlClassG)
public class OWLClassG implements HasUri {

	private static final String CLS_H_FIELD = "owlClassH";

	@Id
	private URI uri;

    @OWLAnnotationProperty(iri = RDFS.LABEL, simpleLiteral = true)
    private String name;

	@OWLObjectProperty(iri = Vocabulary.p_g_hasH, fetch = FetchType.EAGER, cascade = CascadeType.ALL)
	// @ParticipationConstraints({
	// @ParticipationConstraint(owlObjectIRI="http://new.owl#OWLClassA", min=1,
	// max=1)
	// })
	private OWLClassH owlClassH;

    public OWLClassG() {
    }

    public OWLClassG(URI uri) {
        this.uri = uri;
    }

    /**
	 * @param uri
	 *            the uri to set
	 */
	public void setUri(URI uri) {
		this.uri = uri;
	}

	/**
	 * @return the uri
	 */
	public URI getUri() {
		return uri;
	}

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setOwlClassH(OWLClassH owlClassH) {
		this.owlClassH = owlClassH;
	}

	public OWLClassH getOwlClassH() {
		return owlClassH;
	}

	public static String getClassIri() {
		return OWLClassG.class.getAnnotation(OWLClass.class).iri();
	}

    public static Field getNameField() throws NoSuchFieldException {
        return OWLClassG.class.getDeclaredField("name");
    }

	public static Field getOwlClassHField() throws NoSuchFieldException {
		return OWLClassG.class.getDeclaredField(CLS_H_FIELD);
	}
}
