#pragma once

#include <QDialog>
#include "ConfigGeneral.h"

class QButtonGroup;
class QCheckBox;
class QComboBox;
class QDiag;
class QListWidget;
class QListWidgetItem;
class QPushButton;
class QRadioButton;
class QSlider;
class QStackedWidget;

class DConfigDialog : public QDialog
{
	Q_OBJECT

public:
	enum InitialConfigItem
	{
		ICI_General = 0,
		ICI_Graphics = 1,
		ICI_Sound = 2,
		ICI_GCPad = 3,
		ICI_Wiimote = 4,
	};

	DConfigDialog(QWidget* parent = NULL);
	DConfigMainGeneralTab* generalWidget;

public slots:
	void showPage(InitialConfigItem initialConfigItem);
	void switchPage(QListWidgetItem*,QListWidgetItem*);
	void OnReset();
	void OnApply();
	void OnOk();

private:
	void closeEvent(QCloseEvent*);

	QStackedWidget* stackWidget;
	QListWidget* menusView;

signals:
	void Apply();
	void Reset();
	void IsoPathsChanged();
};
