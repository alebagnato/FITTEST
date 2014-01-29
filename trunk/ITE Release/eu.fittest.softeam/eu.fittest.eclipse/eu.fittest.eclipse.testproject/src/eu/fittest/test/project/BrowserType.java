/**
 */
package eu.fittest.test.project;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.util.AbstractEnumerator;

/**
 * <!-- begin-user-doc -->
 * A representation of the literals of the enumeration '<em><b>Browser Type</b></em>',
 * and utility methods for working with them.
 * <!-- end-user-doc -->
 * @see eu.fittest.test.project.ProjectPackage#getBrowserType()
 * @model extendedMetaData="name='BrowserType'"
 * @generated
 */
public final class BrowserType extends AbstractEnumerator {
	/**
	 * The '<em><b>Html Unit Driver</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Html Unit Driver</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #HTML_UNIT_DRIVER_LITERAL
	 * @model name="HtmlUnitDriver"
	 * @generated
	 * @ordered
	 */
	public static final int HTML_UNIT_DRIVER = 0;

	/**
	 * The '<em><b>Firefox Driver</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Firefox Driver</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #FIREFOX_DRIVER_LITERAL
	 * @model name="FirefoxDriver"
	 * @generated
	 * @ordered
	 */
	public static final int FIREFOX_DRIVER = 1;

	/**
	 * The '<em><b>Chrome Driver</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Chrome Driver</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #CHROME_DRIVER_LITERAL
	 * @model name="ChromeDriver"
	 * @generated
	 * @ordered
	 */
	public static final int CHROME_DRIVER = 2;

	/**
	 * The '<em><b>Internet Explorer Driver</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Internet Explorer Driver</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #INTERNET_EXPLORER_DRIVER_LITERAL
	 * @model name="InternetExplorerDriver"
	 * @generated
	 * @ordered
	 */
	public static final int INTERNET_EXPLORER_DRIVER = 3;

	/**
	 * The '<em><b>Flex Object Driver</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Flex Object Driver</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #FLEX_OBJECT_DRIVER_LITERAL
	 * @model name="FlexObjectDriver"
	 * @generated
	 * @ordered
	 */
	public static final int FLEX_OBJECT_DRIVER = 4;

	/**
	 * The '<em><b>Flash Application</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Flash Application</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #FLASH_APPLICATION_LITERAL
	 * @model name="FlashApplication"
	 * @generated
	 * @ordered
	 */
	public static final int FLASH_APPLICATION = 5;

	/**
	 * The '<em><b>Html Unit Driver</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #HTML_UNIT_DRIVER
	 * @generated
	 * @ordered
	 */
	public static final BrowserType HTML_UNIT_DRIVER_LITERAL = new BrowserType(HTML_UNIT_DRIVER, "HtmlUnitDriver", "HtmlUnitDriver");

	/**
	 * The '<em><b>Firefox Driver</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #FIREFOX_DRIVER
	 * @generated
	 * @ordered
	 */
	public static final BrowserType FIREFOX_DRIVER_LITERAL = new BrowserType(FIREFOX_DRIVER, "FirefoxDriver", "FirefoxDriver");

	/**
	 * The '<em><b>Chrome Driver</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #CHROME_DRIVER
	 * @generated
	 * @ordered
	 */
	public static final BrowserType CHROME_DRIVER_LITERAL = new BrowserType(CHROME_DRIVER, "ChromeDriver", "ChromeDriver");

	/**
	 * The '<em><b>Internet Explorer Driver</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #INTERNET_EXPLORER_DRIVER
	 * @generated
	 * @ordered
	 */
	public static final BrowserType INTERNET_EXPLORER_DRIVER_LITERAL = new BrowserType(INTERNET_EXPLORER_DRIVER, "InternetExplorerDriver", "InternetExplorerDriver");

	/**
	 * The '<em><b>Flex Object Driver</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #FLEX_OBJECT_DRIVER
	 * @generated
	 * @ordered
	 */
	public static final BrowserType FLEX_OBJECT_DRIVER_LITERAL = new BrowserType(FLEX_OBJECT_DRIVER, "FlexObjectDriver", "FlexObjectDriver");

	/**
	 * The '<em><b>Flash Application</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #FLASH_APPLICATION
	 * @generated
	 * @ordered
	 */
	public static final BrowserType FLASH_APPLICATION_LITERAL = new BrowserType(FLASH_APPLICATION, "FlashApplication", "FlashApplication");

	/**
	 * An array of all the '<em><b>Browser Type</b></em>' enumerators.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static final BrowserType[] VALUES_ARRAY =
		new BrowserType[] {
			HTML_UNIT_DRIVER_LITERAL,
			FIREFOX_DRIVER_LITERAL,
			CHROME_DRIVER_LITERAL,
			INTERNET_EXPLORER_DRIVER_LITERAL,
			FLEX_OBJECT_DRIVER_LITERAL,
			FLASH_APPLICATION_LITERAL,
		};

	/**
	 * A public read-only list of all the '<em><b>Browser Type</b></em>' enumerators.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static final List VALUES = Collections.unmodifiableList(Arrays.asList(VALUES_ARRAY));

	/**
	 * Returns the '<em><b>Browser Type</b></em>' literal with the specified literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static BrowserType get(String literal) {
		for (int i = 0; i < VALUES_ARRAY.length; ++i) {
			BrowserType result = VALUES_ARRAY[i];
			if (result.toString().equals(literal)) {
				return result;
			}
		}
		return null;
	}

	/**
	 * Returns the '<em><b>Browser Type</b></em>' literal with the specified name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static BrowserType getByName(String name) {
		for (int i = 0; i < VALUES_ARRAY.length; ++i) {
			BrowserType result = VALUES_ARRAY[i];
			if (result.getName().equals(name)) {
				return result;
			}
		}
		return null;
	}

	/**
	 * Returns the '<em><b>Browser Type</b></em>' literal with the specified integer value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static BrowserType get(int value) {
		switch (value) {
			case HTML_UNIT_DRIVER: return HTML_UNIT_DRIVER_LITERAL;
			case FIREFOX_DRIVER: return FIREFOX_DRIVER_LITERAL;
			case CHROME_DRIVER: return CHROME_DRIVER_LITERAL;
			case INTERNET_EXPLORER_DRIVER: return INTERNET_EXPLORER_DRIVER_LITERAL;
			case FLEX_OBJECT_DRIVER: return FLEX_OBJECT_DRIVER_LITERAL;
			case FLASH_APPLICATION: return FLASH_APPLICATION_LITERAL;
		}
		return null;
	}

	/**
	 * Only this class can construct instances.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private BrowserType(int value, String name, String literal) {
		super(value, name, literal);
	}

} //BrowserType
