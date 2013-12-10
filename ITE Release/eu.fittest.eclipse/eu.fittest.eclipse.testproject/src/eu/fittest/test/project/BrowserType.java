/**
 */
package eu.fittest.test.project;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.util.Enumerator;

/**
 * <!-- begin-user-doc -->
 * A representation of the literals of the enumeration '<em><b>Browser Type</b></em>',
 * and utility methods for working with them.
 * <!-- end-user-doc -->
 * @see eu.fittest.test.project.ProjectPackage#getBrowserType()
 * @model extendedMetaData="name='BrowserType'"
 * @generated
 */
public enum BrowserType implements Enumerator {
	/**
	 * The '<em><b>Html Unit Driver</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #HTML_UNIT_DRIVER_VALUE
	 * @generated
	 * @ordered
	 */
	HTML_UNIT_DRIVER(0, "HtmlUnitDriver", "HtmlUnitDriver"),

	/**
	 * The '<em><b>Firefox Driver</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #FIREFOX_DRIVER_VALUE
	 * @generated
	 * @ordered
	 */
	FIREFOX_DRIVER(1, "FirefoxDriver", "FirefoxDriver"),

	/**
	 * The '<em><b>Chrome Driver</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #CHROME_DRIVER_VALUE
	 * @generated
	 * @ordered
	 */
	CHROME_DRIVER(2, "ChromeDriver", "ChromeDriver"),

	/**
	 * The '<em><b>Internet Explorer Driver</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #INTERNET_EXPLORER_DRIVER_VALUE
	 * @generated
	 * @ordered
	 */
	INTERNET_EXPLORER_DRIVER(3, "InternetExplorerDriver", "InternetExplorerDriver"),

	/**
	 * The '<em><b>Flex Object Driver</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #FLEX_OBJECT_DRIVER_VALUE
	 * @generated
	 * @ordered
	 */
	FLEX_OBJECT_DRIVER(4, "FlexObjectDriver", "FlexObjectDriver"),

	/**
	 * The '<em><b>Flash Application</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #FLASH_APPLICATION_VALUE
	 * @generated
	 * @ordered
	 */
	FLASH_APPLICATION(5, "FlashApplication", "FlashApplication");

	/**
	 * The '<em><b>Html Unit Driver</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Html Unit Driver</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #HTML_UNIT_DRIVER
	 * @model name="HtmlUnitDriver"
	 * @generated
	 * @ordered
	 */
	public static final int HTML_UNIT_DRIVER_VALUE = 0;

	/**
	 * The '<em><b>Firefox Driver</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Firefox Driver</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #FIREFOX_DRIVER
	 * @model name="FirefoxDriver"
	 * @generated
	 * @ordered
	 */
	public static final int FIREFOX_DRIVER_VALUE = 1;

	/**
	 * The '<em><b>Chrome Driver</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Chrome Driver</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #CHROME_DRIVER
	 * @model name="ChromeDriver"
	 * @generated
	 * @ordered
	 */
	public static final int CHROME_DRIVER_VALUE = 2;

	/**
	 * The '<em><b>Internet Explorer Driver</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Internet Explorer Driver</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #INTERNET_EXPLORER_DRIVER
	 * @model name="InternetExplorerDriver"
	 * @generated
	 * @ordered
	 */
	public static final int INTERNET_EXPLORER_DRIVER_VALUE = 3;

	/**
	 * The '<em><b>Flex Object Driver</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Flex Object Driver</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #FLEX_OBJECT_DRIVER
	 * @model name="FlexObjectDriver"
	 * @generated
	 * @ordered
	 */
	public static final int FLEX_OBJECT_DRIVER_VALUE = 4;

	/**
	 * The '<em><b>Flash Application</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Flash Application</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #FLASH_APPLICATION
	 * @model name="FlashApplication"
	 * @generated
	 * @ordered
	 */
	public static final int FLASH_APPLICATION_VALUE = 5;

	/**
	 * An array of all the '<em><b>Browser Type</b></em>' enumerators.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static final BrowserType[] VALUES_ARRAY =
		new BrowserType[] {
			HTML_UNIT_DRIVER,
			FIREFOX_DRIVER,
			CHROME_DRIVER,
			INTERNET_EXPLORER_DRIVER,
			FLEX_OBJECT_DRIVER,
			FLASH_APPLICATION,
		};

	/**
	 * A public read-only list of all the '<em><b>Browser Type</b></em>' enumerators.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static final List<BrowserType> VALUES = Collections.unmodifiableList(Arrays.asList(VALUES_ARRAY));

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
			case HTML_UNIT_DRIVER_VALUE: return HTML_UNIT_DRIVER;
			case FIREFOX_DRIVER_VALUE: return FIREFOX_DRIVER;
			case CHROME_DRIVER_VALUE: return CHROME_DRIVER;
			case INTERNET_EXPLORER_DRIVER_VALUE: return INTERNET_EXPLORER_DRIVER;
			case FLEX_OBJECT_DRIVER_VALUE: return FLEX_OBJECT_DRIVER;
			case FLASH_APPLICATION_VALUE: return FLASH_APPLICATION;
		}
		return null;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private final int value;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private final String name;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private final String literal;

	/**
	 * Only this class can construct instances.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private BrowserType(int value, String name, String literal) {
		this.value = value;
		this.name = name;
		this.literal = literal;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getValue() {
	  return value;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getName() {
	  return name;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getLiteral() {
	  return literal;
	}

	/**
	 * Returns the literal value of the enumerator, which is its string representation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String toString() {
		return literal;
	}
	
} //BrowserType
