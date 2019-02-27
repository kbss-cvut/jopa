/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.environment;

import java.lang.reflect.Field;
import java.net.URI;

import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;

@OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassK")
public class OWLClassK {

	private static final String CLS_E_FIELD = "owlClassE";

	@Id(generated = true)
	private URI uri;

	@OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasE")
	private OWLClassE owlClassE;

	public URI getUri() {
		return uri;
	}

	public void setUri(URI uri) {
		this.uri = uri;
	}

	public OWLClassE getOwlClassE() {
		return owlClassE;
	}

	public void setOwlClassE(OWLClassE owlClassE) {
		this.owlClassE = owlClassE;
	}

	@Override
	public String toString() {
		return "[OWLClassK: " + uri + ", owlClassE = " + owlClassE + "]";
	}

	public static String getClassIri() {
		return OWLClassK.class.getAnnotation(OWLClass.class).iri();
	}

	public static Field getOwlClassEField() throws NoSuchFieldException, SecurityException {
		return OWLClassK.class.getDeclaredField(CLS_E_FIELD);
	}
}
