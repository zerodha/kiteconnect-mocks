// To parse this JSON:
//
//   NSError *error;
//   QTMFSIPInfo *mFSIPInfo = [QTMFSIPInfo fromJSON:json encoding:NSUTF8Encoding error:&error];

#import <Foundation/Foundation.h>

@class QTMFSIPInfo;
@class QTDefinitions;
@class QTData;
@class QTDataProperties;
@class QTCompletedInstalments;
@class QTType;
@class QTCreated;
@class QTStepUp;
@class QTMFSIPInfoClass;
@class QTMFSIPInfoProperties;
@class QTStepUpClass;
@class QTStepUpProperties;

NS_ASSUME_NONNULL_BEGIN

#pragma mark - Boxed enums

@interface QTType : NSObject
@property (nonatomic, readonly, copy) NSString *value;
+ (instancetype _Nullable)withValue:(NSString *)value;
+ (QTType *)integer;
+ (QTType *)null;
+ (QTType *)number;
+ (QTType *)string;
@end

#pragma mark - Object interfaces

@interface QTMFSIPInfo : NSObject
@property (nonatomic, copy)   NSString *ref;
@property (nonatomic, copy)   NSString *schema;
@property (nonatomic, strong) QTDefinitions *definitions;

+ (_Nullable instancetype)fromJSON:(NSString *)json encoding:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
+ (_Nullable instancetype)fromData:(NSData *)data error:(NSError *_Nullable *)error;
- (NSString *_Nullable)toJSON:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
- (NSData *_Nullable)toData:(NSError *_Nullable *)error;
@end

@interface QTDefinitions : NSObject
@property (nonatomic, strong) QTData *data;
@property (nonatomic, strong) QTMFSIPInfoClass *mfsipInfo;
@property (nonatomic, strong) QTStepUpClass *stepUp;
@end

@interface QTData : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTDataProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTDataProperties : NSObject
@property (nonatomic, strong) QTCompletedInstalments *completedInstalments;
@property (nonatomic, strong) QTCreated *created;
@property (nonatomic, strong) QTCompletedInstalments *dividendType;
@property (nonatomic, strong) QTCompletedInstalments *frequency;
@property (nonatomic, strong) QTCompletedInstalments *fund;
@property (nonatomic, strong) QTCompletedInstalments *fundSource;
@property (nonatomic, strong) QTCompletedInstalments *instalmentAmount;
@property (nonatomic, strong) QTCompletedInstalments *instalmentDay;
@property (nonatomic, strong) QTCompletedInstalments *instalments;
@property (nonatomic, strong) QTCreated *lastInstalment;
@property (nonatomic, strong) QTCreated *nextInstalment;
@property (nonatomic, strong) QTCompletedInstalments *pendingInstalments;
@property (nonatomic, strong) QTCompletedInstalments *sipID;
@property (nonatomic, strong) QTCompletedInstalments *sipRegNum;
@property (nonatomic, strong) QTCompletedInstalments *sipType;
@property (nonatomic, strong) QTCompletedInstalments *status;
@property (nonatomic, strong) QTStepUp *stepUp;
@property (nonatomic, strong) QTCompletedInstalments *tag;
@property (nonatomic, strong) QTCompletedInstalments *tradingsymbol;
@property (nonatomic, strong) QTCompletedInstalments *transactionType;
@property (nonatomic, strong) QTCompletedInstalments *triggerPrice;
@end

@interface QTCompletedInstalments : NSObject
@property (nonatomic, assign) QTType *type;
@end

@interface QTCreated : NSObject
@property (nonatomic, copy)   NSString *format;
@property (nonatomic, assign) QTType *type;
@end

@interface QTStepUp : NSObject
@property (nonatomic, copy) NSString *ref;
@end

@interface QTMFSIPInfoClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTMFSIPInfoProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTMFSIPInfoProperties : NSObject
@property (nonatomic, strong) QTStepUp *data;
@property (nonatomic, strong) QTCompletedInstalments *status;
@end

@interface QTStepUpClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTStepUpProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTStepUpProperties : NSObject
@property (nonatomic, strong) QTCompletedInstalments *the1502;
@end

NS_ASSUME_NONNULL_END
