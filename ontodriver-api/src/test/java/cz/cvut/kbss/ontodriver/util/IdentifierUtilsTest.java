package cz.cvut.kbss.ontodriver.util;

import org.junit.Test;

import java.net.URI;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.*;

public class IdentifierUtilsTest {

    @Test
    public void generateIdentifierAppendsInstanceIdentifierToUriWithHashFragment() {
        final URI clsUri = URI.create("http://onto.fel.cvut.cz/ontologies/jopa#IdentifierUtilsTest");
        final URI result = IdentifierUtils.generateIdentifier(clsUri);
        assertThat(result.toString(), containsString("_instance"));
    }

    @Test
    public void generateIdentifierAppendsIdentifierToUriEndingWithSlash() {
        final URI clsUri = URI.create("http://onto.fel.cvut.cz/ontologies/jopa/ClassA/");
        final URI result = IdentifierUtils.generateIdentifier(clsUri);
        assertThat(result.toString(), containsString("/instance"));
    }

    @Test
    public void generateIdentifierAppendsIdentifierWithSlashToUriWithoutHashFragment() {
        final URI clsUri = URI.create("http://onto.fel.cvut.cz/ontologies/jopa/ClassA");
        final URI result = IdentifierUtils.generateIdentifier(clsUri);
        assertThat(result.toString(), containsString("/instance"));
    }
}