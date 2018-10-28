package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.environment.OWLClassN;
import cz.cvut.kbss.jopa.oom.converter.ConverterWrapper;
import cz.cvut.kbss.jopa.oom.converter.InstantConverter;
import cz.cvut.kbss.jopa.oom.converter.ToIntegerConverter;
import cz.cvut.kbss.jopa.oom.converter.ToStringConverter;
import org.junit.Test;

import java.lang.reflect.Field;
import java.time.Instant;
import java.util.Date;
import java.util.Optional;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.*;

public class ConverterResolverTest {

    private ConverterResolver sut = new ConverterResolver(new Converters());

    @Test
    public void resolveConverterReturnsEmptyOptionalForObjectPropertyWithEntityTarget() throws Exception {
        final Field field = OWLClassD.getOwlClassAField();
        final PropertyAttributes pa = mock(PropertyAttributes.class);
        when(pa.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        final Optional<ConverterWrapper<?, ?>> result = sut.resolveConverter(field, pa);
        assertFalse(result.isPresent());
    }

    @Test
    public void resolveConverterReturnsBuiltInIntegerConverterForIntegerDataPropertyField() throws Exception {
        final Field field = OWLClassM.getIntAttributeField();
        final PropertyAttributes pa = mock(PropertyAttributes.class);
        when(pa.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        doReturn(BasicTypeImpl.get(Integer.class)).when(pa).getType();
        final Optional<ConverterWrapper<?, ?>> result = sut.resolveConverter(field, pa);
        assertTrue(result.isPresent());
        assertTrue(result.get().supportsAxiomValueType(Integer.class));
        assertTrue(result.get() instanceof ToIntegerConverter);
    }

    @Test
    public void resolveConverterReturnsEmptyOptionalForDataPropertyWithDateTarget() throws Exception {
        final Field field = OWLClassM.getDateAttributeField();
        final PropertyAttributes pa = mock(PropertyAttributes.class);
        when(pa.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        doReturn(BasicTypeImpl.get(Date.class)).when(pa).getType();
        final Optional<ConverterWrapper<?, ?>> result = sut.resolveConverter(field, pa);
        assertFalse(result.isPresent());
    }

    @Test
    public void resolveConverterReturnsBuiltInInstantConverterForInstantDataPropertyField() throws Exception {
        final Field field = OWLClassM.getDateAttributeField();
        final PropertyAttributes pa = mock(PropertyAttributes.class);
        when(pa.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        doReturn(BasicTypeImpl.get(Instant.class)).when(pa).getType();
        final Optional<ConverterWrapper<?, ?>> result = sut.resolveConverter(field, pa);
        assertTrue(result.isPresent());
        assertTrue(result.get() instanceof InstantConverter);
    }

    @Test
    public void resolveConverterReturnsBuiltInStringConverterForStringDataProperty() throws Exception {
        final Field field = OWLClassB.getStrAttField();
        final PropertyAttributes pa = mock(PropertyAttributes.class);
        when(pa.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        doReturn(BasicTypeImpl.get(String.class)).when(pa).getType();
        final Optional<ConverterWrapper<?, ?>> result = sut.resolveConverter(field, pa);
        assertTrue(result.isPresent());
        assertTrue(result.get() instanceof ToStringConverter);
    }

    @Test
    public void resolveConverterReturnsBuiltInStringConverterForStringAnnotationProperty() throws Exception {
        final Field field = OWLClassN.getAnnotationPropertyField();
        final PropertyAttributes pa = mock(PropertyAttributes.class);
        when(pa.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.ANNOTATION);
        doReturn(BasicTypeImpl.get(String.class)).when(pa).getType();
        final Optional<ConverterWrapper<?, ?>> result = sut.resolveConverter(field, pa);
        assertTrue(result.isPresent());
        assertTrue(result.get() instanceof ToStringConverter);
    }
}