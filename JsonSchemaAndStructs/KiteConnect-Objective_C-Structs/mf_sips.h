// To parse this JSON:
//
//   NSError *error;
//   QTMFSips *mFSips = [QTMFSips fromJSON:json encoding:NSUTF8Encoding error:&error];

#import <Foundation/Foundation.h>

@class QTMFSips;
@class QTDefinitions;
@class QTDatum;
@class QTDatumProperties;
@class QTCompletedInstalments;
@class QTType;
@class QTCreated;
@class QTSIPRegNum;
@class QTStepUp;
@class QTMFSipsClass;
@class QTMFSipsProperties;
@class QTData;
@class QTItems;

NS_ASSUME_NONNULL_BEGIN

#pragma mark - Boxed enums

@interface QTType : NSObject
@property (nonatomic, readonly, copy) NSString *value;
+ (instancetype _Nullable)withValue:(NSString *)value;
+ (QTType *)integer;
+ (QTType *)number;
+ (QTType *)string;
@end

#pragma mark - Object interfaces

@interface QTMFSips : NSObject
@property (nonatomic, copy)   NSString *ref;
@property (nonatomic, copy)   NSString *schema;
@property (nonatomic, strong) QTDefinitions *definitions;

+ (_Nullable instancetype)fromJSON:(NSString *)json encoding:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
+ (_Nullable instancetype)fromData:(NSData *)data error:(NSError *_Nullable *)error;
- (NSString *_Nullable)toJSON:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
- (NSData *_Nullable)toData:(NSError *_Nullable *)error;
@end

@interface QTDefinitions : NSObject
@property (nonatomic, strong) QTDatum *datum;
@property (nonatomic, strong) QTMFSipsClass *mfSips;
@end

@interface QTDatum : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTDatumProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTDatumProperties : NSObject
@property (nonatomic, strong) QTCompletedInstalments *completedInstalments;
@property (nonatomic, strong) QTCreated *created;
@property (nonatomic, strong) QTCompletedInstalments *dividendType;
@property (nonatomic, strong) QTCompletedInstalments *frequency;
@property (nonatomic, strong) QTCompletedInstalments *fund;
@property (nonatomic, strong) QTCompletedInstalments *instalmentAmount;
@property (nonatomic, strong) QTCompletedInstalments *instalmentDay;
@property (nonatomic, strong) QTCompletedInstalments *instalments;
@property (nonatomic, strong) QTCreated *lastInstalment;
@property (nonatomic, strong) QTCreated *nextInstalment;
@property (nonatomic, strong) QTCompletedInstalments *pendingInstalments;
@property (nonatomic, strong) QTCompletedInstalments *sipID;
@property (nonatomic, strong) QTSIPRegNum *sipRegNum;
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
@property (nonatomic, nullable, copy) NSString *format;
@property (nonatomic, copy)           NSString *type;
@end

@interface QTSIPRegNum : NSObject
@property (nonatomic, copy) NSArray<QTCreated *> *anyOf;
@end

@interface QTStepUp : NSObject
@property (nonatomic, strong) QTCompletedInstalments *additionalProperties;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTMFSipsClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTMFSipsProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTMFSipsProperties : NSObject
@property (nonatomic, strong) QTData *data;
@end

@interface QTData : NSObject
@property (nonatomic, strong) QTItems *items;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTItems : NSObject
@property (nonatomic, copy) NSString *ref;
@end

NS_ASSUME_NONNULL_END
